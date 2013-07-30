//
//  SDRubyObject.m
//  Zephyros
//
//  Created by Steven on 7/29/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDRubyObject.h"

#import "SDRuby.h"

#import "SDLogWindowController.h"

@interface SDRubyObject () {
    VALUE* internalValue;
}
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
    
    rb_ary_push(outer_array, *internalValue);
    rb_ary_push(outer_array, SDObjcToRubyValue(args));
    
    int err;
    rb_protect(sd_trampoline, outer_array, &err);
    
    if (err) {
        VALUE exception = rb_gv_get("$!");
        VALUE excStr = rb_obj_as_string(exception);
        NSString* exceptionString = [NSString stringWithUTF8String:StringValueCStr(excStr)];
        
        [[SDLogWindowController sharedLogWindowController] show:exceptionString
                                                           type:SDLogMessageTypeError];
    }
}

+ (SDRubyObject*) withRubyValue:(VALUE*)val {
    return [[SDRubyObject alloc] initWithRubyValue:val];
}

- (id) initWithRubyValue:(VALUE*)val {
    if (self = [super init]) {
        internalValue = malloc(sizeof(VALUE));
        *internalValue = *val;
        rb_gc_register_address(internalValue);
    }
    return self;
}


- (void) dealloc {
    rb_gc_unregister_address(internalValue);
    free(internalValue);
}

@end
