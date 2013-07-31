//
//  NuBlock+Callback.m
//  Zephyros
//
//  Created by Steven on 7/30/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "NuBlock+Callback.h"

@implementation NuBlock (Callback)

- (void) call:(NSArray*)args {
    [self evalWithArguments:[args list] context:[self context]];
}

@end
