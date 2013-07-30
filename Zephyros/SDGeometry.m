//
//  SDGeometry.m
//  Zephyros
//
//  Created by Steven on 7/29/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDGeometry.h"

@implementation SDRect

- (void) integralize {
    CGRect r = CGRectFromSDRect(self);
    r = CGRectIntegral(r);
    self.x = @(r.origin.x);
    self.y = @(r.origin.y);
    self.w = @(r.size.width);
    self.h = @(r.size.height);
}

@end

@implementation SDPoint
@end

@implementation SDSize
@end

SDRect* SDRectFromCGRect(CGRect r) {
    SDRect* n = [[SDRect alloc] init];
    n.x = @(r.origin.x);
    n.y = @(r.origin.y);
    n.w = @(r.size.width);
    n.h = @(r.size.height);
    return n;
}

SDPoint* SDPointFromCGPoint(CGPoint r) {
    SDPoint* p = [[SDPoint alloc] init];
    p.x = @(r.x);
    p.y = @(r.y);
    return p;
}

SDSize* SDSizeFromCGSize(CGSize r) {
    SDSize* s = [[SDSize alloc] init];
    s.w = @(r.width);
    s.h = @(r.height);
    return s;
}

CGRect CGRectFromSDRect(SDRect* d) {
    CGRect r;
    r.origin = CGPointFromSDPoint((SDPoint*)d);
    r.size = CGSizeFromSDSize((SDSize*)d);
    return r;
}

CGPoint CGPointFromSDPoint(SDPoint* d) {
    return CGPointMake([d.x doubleValue],
                       [d.y doubleValue]);
}

CGSize CGSizeFromSDSize(SDSize* d) {
    return CGSizeMake([d.w doubleValue],
                      [d.h doubleValue]);
}
