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

NSString* sd_js_coffescript();
NSString* sd_js_underscore();
NSString* sd_js_api();


@interface SDClient : NSObject
@property (retain) JSCocoa* js;
@property (retain) GCDAsyncSocket* sock;
@property uint64_t maxMsgId;
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

- (void) sendRawMessage:(id)msg {
    uint64_t msgid = ++self.maxMsgId;
    
    NSMutableArray* newMsg = [[msg mutableCopy] autorelease];
    [newMsg insertObject:@(msgid) atIndex:0];
    
    NSData* msgData = [NSJSONSerialization dataWithJSONObject:newMsg options:0 error:NULL];
    NSString* msgLength = [NSString stringWithFormat:@"%ld", [msgData length]];
    
    [self.sock writeData:[msgLength dataUsingEncoding:NSUTF8StringEncoding] withTimeout:3 tag:0];
    [self.sock writeData:[GCDAsyncSocket LFData] withTimeout:3 tag:0];
    [self.sock writeData:msgData withTimeout:3 tag:0];
}

- (void) alert:(NSString*)msg delay:(NSNumber*)delay {
    [self sendRawMessage:@[@0, @"alert", msg, delay]];
}

- (id) evalCoffeeScript:(NSString*)coffee {
    NSString* js = [self.js callFunction:@"coffeeToJS" withArguments:@[coffee]];
    return [self.js eval:js];
}

- (void) loadFile:(NSData*)contentsData isCoffee:(BOOL)isCoffee {
    NSString* contents = [[NSString alloc] initWithData:contentsData encoding:NSUTF8StringEncoding];
    
    if (isCoffee) {
        [self evalCoffeeScript:contents];
    }
    else {
        [self.js evalJSString:contents];
    }
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
    
//    [self evalCoffeeScript:sd_js_api()];
    
    self.sock = [[[GCDAsyncSocket alloc] initWithDelegate:self delegateQueue:dispatch_get_main_queue()] autorelease];
    
    [self.sock connectToHost:@"localhost" onPort:1235 error:NULL];
    
//    NSLog(@"%@", [self evalCoffeeScript:@"SDClient.sharedClient().alert()"]);
}

//- (void)socket:(GCDAsyncSocket *)sock didConnectToHost:(NSString *)host port:(uint16_t)port {
//}

@end




int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString* file = nil;
        BOOL coffee = NO;
        
        if (argc == 2) {
            file = [NSString stringWithUTF8String:argv[1]];
        }
        else if (argc == 3) {
            file = [NSString stringWithUTF8String:argv[1]];
            coffee = YES;
        }
        else {
            printf("usage: %s script.js\n"
                   "       %s -coffee script.coffee", argv[0], argv[0]);
            return 1;
        }
        
        NSData* contentsData = [[NSFileManager defaultManager] contentsAtPath:file];
        if (contentsData == nil) {
            printf("Couldn't read file: %s\n", [file UTF8String]);
            printf("Are you sure it exists? Maybe you made a typo?\n");
            return 0;
        }
        
        [[SDClient sharedClient] setup];
        [[SDClient sharedClient] loadFile:contentsData isCoffee:coffee];
        [[NSRunLoop mainRunLoop] run];
    }
    return 0;
}
