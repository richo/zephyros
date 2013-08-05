//
//  SDZephJS.h
//  zephjs
//
//  Created by Steven on 8/5/13.
//  Copyright (c) 2013 Steven Degutis. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SDZephJS : NSObject

+ (SDZephJS*) sharedZeph;
- (void) setup;
- (void) evalFile:(NSData*)contentsData asCoffee:(BOOL)isCoffee;

@end
