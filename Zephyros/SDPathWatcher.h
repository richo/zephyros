//
//  SDConfigWatcher.h
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SDPathWatcher : NSObject

- (void) stopWatching;
- (void) startWatching:(NSArray*)pathsToWatch;

@end
