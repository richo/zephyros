//
//  BindkeyOp.h
//  Zephyros
//
//  Created by Steven on 4/13/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#import "SDCallback.h"

@interface SDKeyBinder : NSObject

+ (SDKeyBinder*) sharedKeyBinder;

- (void) bind:(NSString*)key modifiers:(NSArray*)mods fn:(dispatch_block_t)fn;

- (void) removeKeyBindings;
- (NSArray*) finalizeNewKeyBindings;

@end
