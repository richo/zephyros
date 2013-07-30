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


VALUE sd_trampoline(VALUE obj) {
    VALUE* ptr = RARRAY_PTR(obj);
    return rb_apply(ptr[0], rb_intern("call"), ptr[1]);
}


@implementation SDRubyObject

- (void) call:(NSArray*)args {
    if (args == nil)
        args = @[];
    
    VALUE outer_array = rb_ary_new();
    
    rb_ary_push(outer_array, self.internalValue);
    rb_ary_push(outer_array, SDObjcToRubyValue(args));
    
    int err;
    rb_protect(sd_trampoline, outer_array, &err);
    
    if (err) {
        VALUE exception = rb_gv_get("$!");
        VALUE excStr = rb_obj_as_string(exception);
        NSString* exceptionString = [NSString stringWithUTF8String:StringValueCStr(excStr)];
        NSLog(@"crap: %@", exceptionString);
    }
}

+ (SDRubyObject*) withRubyValue:(VALUE)val {
    SDRubyObject* block = [[SDRubyObject alloc] init];
    block.internalValue = val;
    return block;
}

@end
