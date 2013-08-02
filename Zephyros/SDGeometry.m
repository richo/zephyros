//
//  SDGeometry.m
//  Zephyros
//
//  Created by Steven on 7/29/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDGeometry.h"

NSDictionary* SDDictFromRect(CGRect r) {
    NSMutableDictionary* d = [NSMutableDictionary dictionary];
    [d addEntriesFromDictionary:SDDictFromPoint(r.origin)];
    [d addEntriesFromDictionary:SDDictFromSize(r.size)];
    return d;
}

NSDictionary* SDDictFromPoint(CGPoint r) {
    return @{@"x": @(r.x),
             @"y": @(r.y)};
}

NSDictionary* SDDictFromSize(CGSize r) {
    return @{@"w": @(r.width),
             @"h": @(r.height)};
}

CGRect SDRectFromDict(NSDictionary* d) {
    CGRect r;
    r.origin = SDPointFromDict(d);
    r.size = SDSizeFromDict(d);
    return r;
}

CGPoint SDPointFromDict(NSDictionary* d) {
    return CGPointMake([[d objectForKey:@"x"] doubleValue],
                       [[d objectForKey:@"y"] doubleValue]);
}

CGSize SDSizeFromDict(NSDictionary* d) {
    return CGSizeMake([[d objectForKey:@"w"] doubleValue],
                      [[d objectForKey:@"h"] doubleValue]);
}
