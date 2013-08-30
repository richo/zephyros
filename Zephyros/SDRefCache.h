//
//  SDRefCache.h
//  Zephyros
//
//  Created by Steven Degutis on 8/30/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SDRefCache : NSObject

- (id) refForKey:(id)key;
- (id) storeRef:(id)ref;
- (void) removeRefForKey:(id)key;

// only used for top-level obj
- (void) store:(id)obj withKey:(id)key;

@end
