//
//  SDClient.h
//  Zephyros
//
//  Created by Steven Degutis on 7/31/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "GCDAsyncSocket.h"

@interface SDClient : NSObject

@property GCDAsyncSocket* sock;

- (void) waitForNewMessage;

- (void) sendMessage:(id)msg;

@property (copy) void(^disconnectedHandler)(SDClient* me);

@end
