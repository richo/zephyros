//
//  SDJSBlockWrapper.h
//  Zephyros
//
//  Created by Steven on 4/17/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

#import <JSCocoa/JSCocoa.h>

#import "SDCallback.h"

@interface SDJSCallback : NSObject <SDCallback>

- (id) initWithJavaScriptFn:(JSValueRefAndContextRef)fn;

- (void) call:(NSArray*)args;

@end
