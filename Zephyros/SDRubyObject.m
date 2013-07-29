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
    VALUE rubyargs = SDObjcToRubyValue(args);
    rb_funcall2(self.internalValue, rb_intern("call"), (int)[args count], &rubyargs);
}

+ (SDRubyObject*) withRubyValue:(VALUE)val {
    SDRubyObject* block = [[SDRubyObject alloc] init];
    block.internalValue = val;
    return block;
}

@end
