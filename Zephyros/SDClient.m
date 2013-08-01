//
//  SDClient.m
//  Zephyros
//
//  Created by Steven Degutis on 7/31/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDClient.h"

@implementation SDClient

- (void) waitForNewMessage {
    [self.sock readDataToData:[@"\n" dataUsingEncoding:NSUTF8StringEncoding]
                  withTimeout:3
                          tag:0];
}

- (void)socket:(GCDAsyncSocket *)sock didReadData:(NSData *)data withTag:(long)tag {
    if (tag == 0) {
        NSString* str = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        NSInteger size = [str integerValue];
        
        [self.sock readDataToLength:size
                        withTimeout:3
                                tag:1];
    }
    else if (tag == 1) {
        id obj = [NSJSONSerialization JSONObjectWithData:data options:NSJSONReadingMutableContainers error:0];
        [self handleMessage:obj];
        [self waitForNewMessage];
    }
}

- (void)socketDidDisconnect:(GCDAsyncSocket *)sock withError:(NSError *)err {
    self.disconnectedHandler(self);
}

- (void) handleMessage:(id)msg {
    NSLog(@"new msg: %@", msg);
    
    if ([[msg objectAtIndex:1
          ] isEqual:@3])
        [self sendMessage:[@[@"foobar", @12] mutableCopy]];
    
    [self sendMessage:msg];
}

- (void) sendMessage:(id)msg {
    [msg replaceObjectAtIndex:0 withObject:@"response"];
    
    NSData* data = [NSJSONSerialization dataWithJSONObject:msg options:0 error:NULL];
    NSString* len = [NSString stringWithFormat:@"%ld", [data length]];
    [self.sock writeData:[len dataUsingEncoding:NSUTF8StringEncoding] withTimeout:3 tag:0];
    [self.sock writeData:[GCDAsyncSocket LFData] withTimeout:3 tag:0];
    [self.sock writeData:data withTimeout:3 tag:0];
}

@end
