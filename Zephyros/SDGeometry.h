//
//  SDGeometry.h
//  Zephyros
//
//  Created by Steven on 7/29/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

NSDictionary* SDDictFromRect(CGRect r);
NSDictionary* SDDictFromPoint(CGPoint r);
NSDictionary* SDDictFromSize(CGSize r);

CGRect SDRectFromDict(NSDictionary* d);
CGPoint SDPointFromDict(NSDictionary* d);
CGSize SDSizeFromDict(NSDictionary* d);
