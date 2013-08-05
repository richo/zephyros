//
//  main.m
//  zephjs
//
//  Created by Steven Degutis on 8/4/13.
//  Copyright (c) 2013 Steven Degutis. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "JSCocoa.h"
#import "GCDAsyncSocket.h"
#import "SDJSBlockWrapper.h"

NSString* sd_js_coffescript();
NSString* sd_js_underscore();
NSString* sd_js_api();

#define FOREVER (60.0 * 60.0 * 24.0 * 365.0)


// copied verbatim from http://nullpointer.ph/questions/339/how-do-you-implement-a-thread-safe-queue-for-objective-c
@interface SDQueue : NSObject
@property (retain) NSCondition* queueLock;
@property (retain) NSMutableArray* queueContents;
@end

@implementation SDQueue

- (id) init {
    if (self = [super init]) {
        self.queueLock = [[[NSCondition alloc] init] autorelease];
        self.queueContents = [NSMutableArray array];
    }
    return self;
}

- (id) get {
    id toRet = nil;
    [self.queueLock lock];
    @try
    {
    	while ([self.queueContents count] == 0)
    	{
    		[self.queueLock wait];
    	}
        
    	toRet = [self.queueContents lastObject];
    	[self.queueContents removeLastObject];
    }
    @finally
    {
    	[self.queueLock unlock];
    	return toRet;
    }
}

- (void) put:(id) putObject
{
    [self.queueLock lock];
    @try
    {
    	[self.queueContents insertObject:putObject atIndex:0];
    	[self.queueLock broadcast];
    }
    @finally
    {
    	[self.queueLock unlock];
    }
}

@end


@interface SDClient : NSObject
@property (retain) JSCocoa* js;
@property (retain) GCDAsyncSocket* sock;
@property uint64_t maxMsgId;
@property (retain) NSMutableDictionary* queues;
@end

@implementation SDClient

+ (SDClient*) sharedClient {
    static SDClient* sharedClient;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedClient = [[SDClient alloc] init];
    });
    return sharedClient;
}

- (void) sendAsyncMessage:(id)msg responses:(int)responses callback:(void(^)(id obj))callback {
    uint64_t msgid = ++self.maxMsgId;
    
    NSNumber* msgIdNum = @(msgid);
    
    SDQueue* queue = [[[SDQueue alloc] init] autorelease];
    [self.queues setObject:queue forKey:msgIdNum];
    
    NSMutableArray* newMsg = [[msg mutableCopy] autorelease];
    [newMsg insertObject:msgIdNum atIndex:0];
    
    NSData* msgData = [NSJSONSerialization dataWithJSONObject:newMsg options:0 error:NULL];
    NSString* msgLength = [NSString stringWithFormat:@"%ld", [msgData length]];
    
    [self.sock writeData:[msgLength dataUsingEncoding:NSUTF8StringEncoding] withTimeout:3 tag:0];
    [self.sock writeData:[GCDAsyncSocket LFData] withTimeout:3 tag:0];
    [self.sock writeData:msgData withTimeout:3 tag:0];
    
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        if (responses == -1) {
            [queue get]; // ignore first
            while (true) {
                id obj = [queue get];
                callback(obj);
            }
        }
        else {
            for (int i = 0; i < responses; i++) {
                id obj = [queue get];
                callback(obj);
            }
        }
        
        [self.queues removeObjectForKey:msgIdNum];
    });
}

NSString* const DoNotWaitShellOption = @"donotwait";
NSString* const PwdShellOption = @"pwd";
NSString* const InputShellOption = @"input";

- (NSDictionary*) shell:(NSString*)cmd args:(NSArray*)args opts:(NSDictionary*)options {
    BOOL doNotWait = NO;
    NSPipe* outPipe = [NSPipe pipe];
    NSPipe* errPipe = [NSPipe pipe];
    NSPipe* inPipe = [NSPipe pipe];
    
    NSString* pwd = [options objectForKey:PwdShellOption];
    NSString* input = [options objectForKey:InputShellOption];
    NSValue* doNotWaitOption = [options objectForKey:DoNotWaitShellOption];
    if ([doNotWaitOption isKindOfClass:[NSValue class]]) {
        [doNotWaitOption getValue:&doNotWait];
    }
    
    if (input) {
        [[inPipe fileHandleForWriting] writeData:[input dataUsingEncoding:NSUTF8StringEncoding]];
        [[inPipe fileHandleForWriting] closeFile];
    }
    
    NSTask* task = [[NSTask alloc] init];
    task.launchPath = cmd;
    task.arguments = args;
    if (pwd)
        task.currentDirectoryPath = pwd;
    task.standardInput = inPipe;
    task.standardOutput = outPipe;
    task.standardError = errPipe;
    
    if (doNotWait) {
        [[outPipe fileHandleForReading] waitForDataInBackgroundAndNotify];
        [task launch];
        return @{};
    }
    
    [task launch];
    [task waitUntilExit];
    
    NSData* stdoutData = [[outPipe fileHandleForReading] readDataToEndOfFile];
    NSString* stdoutString = [[NSString alloc] initWithData:stdoutData encoding:NSUTF8StringEncoding];
    
    NSData* stderrData = [[errPipe fileHandleForReading] readDataToEndOfFile];
    NSString* stderrString = [[NSString alloc] initWithData:stderrData encoding:NSUTF8StringEncoding];
    
    return @{@"status": @([task terminationStatus]),
             @"stdout": stdoutString,
             @"stderr": stderrString};
}

- (void) requireFromJS:(NSString*)file {
    file = [file stringByStandardizingPath];
    
    NSData* contentsData = [[NSFileManager defaultManager] contentsAtPath:file];
    if (contentsData == nil) {
        printf("Couldn't require file: %s\n", [file UTF8String]);
        fflush(stdout);
        return;
    }
    
    [self evalFile:contentsData asCoffee:[file hasSuffix:@".coffee"]];
}

- (void) doFn:(JSValueRefAndContextRef)fn after:(NSNumber*)delay {
    SDJSBlockWrapper* block = [[[SDJSBlockWrapper alloc] initWithJavaScriptFn:fn] autorelease];
    double delayInSeconds = [delay doubleValue];
    dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, (int64_t)(delayInSeconds * NSEC_PER_SEC));
    dispatch_after(popTime, dispatch_get_main_queue(), ^(void){
        [block call:nil];
    });
}

- (void) sendAsyncMessage:(id)msg responses:(int)responses callbackJSFunc:(JSValueRefAndContextRef)fn {
    SDJSBlockWrapper* block = [[[SDJSBlockWrapper alloc] initWithJavaScriptFn:fn] autorelease];
    
    [self sendAsyncMessage:msg responses:responses callback:^(id obj) {
        if (obj == nil || obj == [NSNull null])
            obj = [NSNull null];
        
        [block call:@[obj]];
    }];
}

- (id) sendSyncMessage:(id)msg {
    __block id returnVal = nil;
    dispatch_semaphore_t sem = dispatch_semaphore_create(0);
    
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        [self sendAsyncMessage:msg responses:1 callback:^(id obj) {
            returnVal = obj;
            dispatch_semaphore_signal(sem);
        }];
    });
    
    dispatch_semaphore_wait(sem, DISPATCH_TIME_FOREVER);
    return returnVal;
}

- (id) evalCoffeeScript:(NSString*)coffee {
    NSString* js = [self.js callFunction:@"coffeeToJS" withArguments:@[coffee]];
    return [self.js eval:js];
}

- (void) evalFile:(NSData*)contentsData asCoffee:(BOOL)isCoffee {
    NSString* contents = [[NSString alloc] initWithData:contentsData encoding:NSUTF8StringEncoding];
    
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        if (isCoffee) {
            [self evalCoffeeScript:contents];
        }
        else {
            [self.js evalJSString:contents];
        }
    });
}

- (void) setup {
    self.queues = [NSMutableDictionary dictionary];
    
    self.js = [JSCocoa new];
    self.js.delegate = self;
    self.js.useSplitCall = NO;
    self.js.useAutoCall = YES;
    self.js.useJSLint = NO;
    self.js.useAutoCall = NO;
    
    [self.js evalJSString:sd_js_coffescript()];
    [self.js evalJSString:sd_js_underscore()];
    [self.js evalJSString:@"function coffeeToJS(coffee) { return CoffeeScript.compile(coffee, { bare: true }); };"];
    [self evalCoffeeScript:sd_js_api()];
    
    self.sock = [[[GCDAsyncSocket alloc] initWithDelegate:self delegateQueue:dispatch_get_main_queue()] autorelease];
    [self.sock connectToHost:@"localhost" onPort:1235 error:NULL];
    
    [self waitForNewMessage];
}

//- (void)socket:(GCDAsyncSocket *)sock didConnectToHost:(NSString *)host port:(uint16_t)port {
//}

- (void) waitForNewMessage {
    [self.sock readDataToData:[GCDAsyncSocket LFData] withTimeout:FOREVER tag:0];
}

- (void)socket:(GCDAsyncSocket *)sock didReadData:(NSData *)data withTag:(long)tag {
    if (tag == 0) {
        NSString* lenStr = [[[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding] autorelease];
//        NSLog(@"got len: %@", lenStr);
        [self.sock readDataToLength:[lenStr integerValue] withTimeout:FOREVER tag:1];
    }
    else if (tag == 1) {
        NSArray* msg = [NSJSONSerialization JSONObjectWithData:data options:0 error:NULL];
        NSNumber* msgId = [msg objectAtIndex:0];
        id value = [msg objectAtIndex:1];
        
//        NSLog(@"got msg #%@: %@", msgId, value);
        
        dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
            SDQueue* queue = [self.queues objectForKey:msgId];
            [queue put:value];
        });
        
        [self waitForNewMessage];
    }
}

@end




int main(int argc, const char * argv[]) {
    @autoreleasepool {
        if (argc != 2) {
            printf("usage: zephjs script.js\n"
                   "       zephjs script.coffee");
            return 1;
        }
        
        NSString* file = [NSString stringWithUTF8String:argv[1]];
        BOOL coffee = [file hasSuffix:@".coffee"];
        
        NSData* contentsData = [[NSFileManager defaultManager] contentsAtPath:file];
        if (contentsData == nil) {
            printf("Couldn't read file: %s\n", [file UTF8String]);
            printf("Are you sure it exists? Maybe you made a typo?\n");
            return 0;
        }
        
        [[SDClient sharedClient] setup];
        [[SDClient sharedClient] evalFile:contentsData asCoffee:coffee];
        dispatch_main();
    }
    return 0;
}
