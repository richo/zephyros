//
//  SDZephJS.m
//  zephjs
//
//  Created by Steven on 8/5/13.
//  Copyright (c) 2013 Steven Degutis. All rights reserved.
//

#import "SDZephJS.h"

#import "JSCocoa.h"
#import "SDZephClient.h"
#import "SDJSBlockWrapper.h"

NSString* sd_js_coffescript();
NSString* sd_js_underscore();
NSString* sd_js_api();

@interface SDZephJS ()

@property (retain) JSCocoa* js;
@property (retain) SDZephClient* client;

@end


@implementation SDZephJS

+ (SDZephJS*) sharedZeph {
    static SDZephJS* sharedZeph;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedZeph = [[SDZephJS alloc] init];
    });
    return sharedZeph;
}

- (void) setup {
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

    dispatch_block_t errorBlock = ^{
        printf("Can't connect. Is Zephyros running?\n");
        exit(1);
    };

    self.client = [[SDZephClient alloc] init];
    self.client.errorCallback = ^(NSError* err) {
        errorBlock();
    };

    if (![self.client connect]) {
        errorBlock();
    }
}

- (NSDictionary*) shell:(NSString*)cmd args:(NSArray*)args opts:(NSDictionary*)options {
    BOOL doNotWait = NO;
    NSPipe* outPipe = [NSPipe pipe];
    NSPipe* errPipe = [NSPipe pipe];
    NSPipe* inPipe = [NSPipe pipe];

    NSString* pwd = [options objectForKey:@"pwd"];
    NSString* input = [options objectForKey:@"input"];
    NSValue* doNotWaitOption = [options objectForKey:@"donotwait"];
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
    NSString* stdoutString = [[[NSString alloc] initWithData:stdoutData encoding:NSUTF8StringEncoding] autorelease];

    NSData* stderrData = [[errPipe fileHandleForReading] readDataToEndOfFile];
    NSString* stderrString = [[[NSString alloc] initWithData:stderrData encoding:NSUTF8StringEncoding] autorelease];

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
    dispatch_after(popTime, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^(void){
        [block call:nil];
    });
}

- (id) sendSyncMessage:(id)msg {
    return [self.client sendSyncMessage:msg];
}

- (void) sendAsyncMessage:(id)msg responses:(int)responses callbackJSFunc:(JSValueRefAndContextRef)fn {
    SDJSBlockWrapper* block = [[[SDJSBlockWrapper alloc] initWithJavaScriptFn:fn] autorelease];

    [self.client sendAsyncMessage:msg responses:responses callback:^(id obj) {
        if (obj == nil || obj == [NSNull null])
            obj = [NSNull null];

        [block call:@[obj]];
    }];
}

- (id) evalCoffeeScript:(NSString*)coffee {
    NSString* js = [self.js callFunction:@"coffeeToJS" withArguments:@[coffee]];
    return [self.js eval:js];
}

- (void) evalFile:(NSData*)contentsData asCoffee:(BOOL)isCoffee {
    NSString* contents = [[NSString alloc] initWithData:contentsData encoding:NSUTF8StringEncoding];

    if (isCoffee) {
        [self evalCoffeeScript:contents];
    }
    else {
        [self.js evalJSString:contents];
    }
}

@end
