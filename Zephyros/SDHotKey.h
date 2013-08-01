//
//  BindkeyOp.h
//  Zephyros
//
//  Created by Steven on 4/13/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface SDHotKey : NSObject

@property NSArray* modifiers;
@property NSString* key;
@property (copy) dispatch_block_t fn;

- (BOOL) bind;
- (void) unbind;
- (NSString*) hotKeyDescription;

@end
