//
//  SDTopLevelClientProxy.m
//  Zephyros
//
//  Created by Steven Degutis on 8/12/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDTopLevelClientProxy.h"

@implementation SDTopLevelClientProxy

+ (id) method:(SEL)sel args:(NSArray*)types {
    return nil;
}

+ (NSDictionary*) mappings {
    return @{@"bind": [self method:@selector(bind:mods:) args:@[[NSString self], [NSArray self]]]};
}

- (id) bind:(NSString*)key mods:(NSArray*)mods {
    return nil;
}

@end
