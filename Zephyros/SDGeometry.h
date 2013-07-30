//
//  SDGeometry.h
//  Zephyros
//
//  Created by Steven on 7/29/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SDPoint : NSObject
@property NSNumber* x;
@property NSNumber* y;
@end

@interface SDSize : NSObject
@property NSNumber* w;
@property NSNumber* h;
@end

@interface SDRect : NSObject
@property NSNumber* x;
@property NSNumber* y;
@property NSNumber* w;
@property NSNumber* h;
@end

SDRect* SDRectFromCGRect(CGRect r);
SDPoint* SDPointFromCGPoint(CGPoint r);
SDSize* SDSizeFromCGSize(CGSize r);

CGRect CGRectFromSDRect(SDRect* d);
CGPoint CGPointFromSDPoint(SDPoint* d);
CGSize CGSizeFromSDSize(SDSize* d);






NSDictionary* SDDictFromRect(CGRect r);
NSDictionary* SDDictFromPoint(CGPoint r);
NSDictionary* SDDictFromSize(CGSize r);

CGRect SDRectFromDict(NSDictionary* d);
CGPoint SDPointFromDict(NSDictionary* d);
CGSize SDSizeFromDict(NSDictionary* d);
