//
//  SDZephClient.h
//  zephjs
//
//  Created by Steven on 8/5/13.
//  Copyright (c) 2013 Steven Degutis. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SDZephClient : NSObject

- (BOOL) connect;

- (id) sendSyncMessage:(id)msg;
- (void) sendAsyncMessage:(id)msg responses:(int)responses callback:(void(^)(id obj))callback;

@property (copy) void(^errorCallback)(NSError* error);

@end
