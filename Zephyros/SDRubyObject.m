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
    int count = (int)[args count];
    VALUE* rubyargs = malloc(sizeof(VALUE) * count);
    
    int i = 0;
    for (id arg in args) {
        rubyargs[i++] = SDObjcToRubyValue(arg);
    }
    
    rb_funcall2(self.internalValue, rb_intern("call"), count, rubyargs);
    
    free(rubyargs);
}

+ (SDRubyObject*) withRubyValue:(VALUE)val {
    SDRubyObject* block = [[SDRubyObject alloc] init];
    block.internalValue = val;
    return block;
}

@end
