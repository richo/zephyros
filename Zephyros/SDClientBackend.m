//
//  SDClient.m
//  Zephyros
//
//  Created by Steven Degutis on 7/31/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDClientBackend.h"

#import "SDClient.h"
#import "SDLogWindowController.h"

#define FOREVER (60*60*24*365)

@interface SDClientBackend ()

@property SDClient* client;

@end


@implementation SDClientBackend

- (id) init {
    if (self = [super init]) {
        self.client = [[SDClient alloc] init];
        self.client.delegate = self;
    }
    return self;
}

- (void) waitForNewMessage {
    [self.sock readDataToData:[@"\n" dataUsingEncoding:NSUTF8StringEncoding]
                  withTimeout:FOREVER
                          tag:0];
}

- (void) sendMessage:(id)msg {
//    NSLog(@"sending [%@]", msg);
    
    NSData* data = [NSJSONSerialization dataWithJSONObject:msg options:0 error:NULL];
    
//    NSString* tempStr = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
    NSString* len = [NSString stringWithFormat:@"%ld", [data length]];
//    NSLog(@"len = %@", len);
//    NSLog(@"data = %@", tempStr);
    [self.sock writeData:[len dataUsingEncoding:NSUTF8StringEncoding] withTimeout:3 tag:0];
    [self.sock writeData:[GCDAsyncSocket LFData] withTimeout:3 tag:0];
    [self.sock writeData:data withTimeout:3 tag:0];
}

- (void)socket:(GCDAsyncSocket *)sock didReadData:(NSData *)data withTag:(long)tag {
    if (tag == 0) {
        NSString* str = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        NSInteger size = [str integerValue];
        
        NSString* sizeValidator = [NSString stringWithFormat:@"%ld\n", size];
        
        if (![sizeValidator isEqualToString:str]) {
            [self showAPIError:[NSString stringWithFormat:@"API Error: expected JSON data-load length, got: %@", str]];
            [self waitForNewMessage];
            return;
        }
        
        [self.sock readDataToLength:size
                        withTimeout:FOREVER
                                tag:1];
    }
    else if (tag == 1) {
        NSError* __autoreleasing error;
        id obj = [NSJSONSerialization JSONObjectWithData:data options:0 error:&error];
        
        if (obj == nil) {
            NSString* rawJson = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
            [self showAPIError:[NSString stringWithFormat:@"API Error: expected valid JSON message, got: %@", rawJson]];
            [self waitForNewMessage];
            return;
        }
        
        [self.client handleMessage:obj];
        [self waitForNewMessage];
    }
}

- (void)socketDidDisconnect:(GCDAsyncSocket *)sock withError:(NSError *)err {
//    NSLog(@"did disconnect");
    [self.client destroy];
    self.disconnectedHandler(self);
}

- (void) showAPIError:(NSString*)errorStr {
    [[SDLogWindowController sharedLogWindowController] show:errorStr
                                                       type:SDLogMessageTypeError];
}

@end
