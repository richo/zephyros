//
//  SDZephClient.h
//  zephjs
//
//  Created by Steven on 8/5/13.
//  Copyright (c) 2013 Steven Degutis. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SDZephClient : NSObject

- (void) connect;

- (id) sendSyncMessage:(id)msg;
- (void) sendAsyncMessage:(id)msg responses:(int)responses callback:(void(^)(id obj))callback;

@end
