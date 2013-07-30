//
//  SDRubyObject.h
//  Zephyros
//
//  Created by Steven on 7/29/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "SDCallback.h"
#import <ruby/ruby.h>

@interface SDRubyObject : NSObject

+ (SDRubyObject*) withRubyValue:(VALUE*)val;

@end
