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

- (void) alert {
    NSString* msg = @"[0, 0, \"alert\", \"a world\", 2]";
    msg = [NSString stringWithFormat:@"%ld\n%@", [msg length], msg];
    NSLog(@"sending %@", msg);
    
    NSData* data = [msg dataUsingEncoding:NSUTF8StringEncoding];
    
    [self.sock writeData:data withTimeout:3 tag:0];
}

- (id) evalCoffeeScript:(NSString*)coffee {
    NSString* js = [self.js callFunction:@"coffeeToJS" withArguments:@[coffee]];
    return [self.js eval:js];
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
    
    self.sock = [[GCDAsyncSocket alloc] initWithDelegate:self delegateQueue:dispatch_get_main_queue()];
    
    [self.sock connectToHost:@"localhost" onPort:1235 error:NULL];
    
    
    NSLog(@"%@", [self evalCoffeeScript:@"SDClient.sharedClient().alert()"]);
    
}

- (void)socket:(GCDAsyncSocket *)sock didConnectToHost:(NSString *)host port:(uint16_t)port {
}

@end




int main(int argc, const char * argv[]) {
    @autoreleasepool {
        [[SDClient sharedClient] setup];
        [[NSRunLoop mainRunLoop] run];
    }
    return 0;
}
