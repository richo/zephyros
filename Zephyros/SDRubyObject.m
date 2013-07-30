//
//  SDRubyObject.m
//  Zephyros
//
//  Created by Steven on 7/29/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDRubyObject.h"

#import "SDRuby.h"

@interface SDRubyObject ()
@property VALUE internalValue;
@end

@implementation SDRubyObject

- (void) call:(NSArray*)args {
    if (args == nil)
        args = @[];
    
    VALUE args2 = SDObjcToRubyValue(args);
    ID method = rb_intern("call");
    
    rb_apply(self.internalValue, method, args2);
}

+ (SDRubyObject*) withRubyValue:(VALUE)val {
    SDRubyObject* block = [[SDRubyObject alloc] init];
    block.internalValue = val;
    return block;
}

@end
