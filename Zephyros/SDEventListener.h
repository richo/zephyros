//
//  SDEventListener.h
//  Zephyros
//
//  Created by Steven on 4/21/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SDEventListener : NSObject

@property NSString* eventName;
@property (copy) void(^fn)(id thing);

- (void) startListening;
- (void) stopListening;

@end
