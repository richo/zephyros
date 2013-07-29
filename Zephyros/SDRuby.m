//
//  SDRuby.m
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDRuby.h"


#include <ruby/ruby.h>




VALUE sd_method_missing(VALUE self, VALUE args) {
    NSString* s;
    Data_Get_Struct(self, __bridge_transfer NSString, s);
    
    NSLog(@"im called! [%@]", s);
    return Qnil;
}



VALUE sd_bind_keys_fn(VALUE module, VALUE key, VALUE mods) {
    VALUE p = rb_block_proc();
    
    
    
    rb_funcall(p, rb_intern("call"), 0);
    
    NSString* ss = [NSString stringWithUTF8String:StringValueCStr(key)];
    NSLog(@"it was [%@]", ss);

//    double delayInSeconds = 2.0;
//    dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, (int64_t)(delayInSeconds * NSEC_PER_SEC));
//    dispatch_after(popTime, dispatch_get_main_queue(), ^(void){
//        rb_funcall(callback, rb_intern("call"), 0);
//    });
//    
//
    NSLog(@"lolcat");
    return Qnil;
}



@implementation SDRuby

- (void) setup {
    RUBY_INIT_STACK;
    ruby_init();
    ruby_init_loadpath();
    
    VALUE api_module = rb_define_module("Internal");
    rb_define_module_function(api_module, "bind", sd_bind_keys_fn, 2);
    
    VALUE c = rb_define_class("ObjcWrapper", rb_cObject);
    rb_define_method(c, "method_missing", RUBY_METHOD_FUNC(sd_method_missing), -2);
    
    rb_require([[[NSBundle mainBundle] pathForResource:@"api" ofType:@"rb"] UTF8String]);
    
    
    
    
    
    
    NSString* myptr = @"this is awesomes";
    VALUE cc = rb_eval_string("Window");
    VALUE wrapped = Data_Wrap_Struct(cc, NULL, NULL, (__bridge_retained void*)myptr);

    rb_iv_set(rb_eval_string("self"), "@something", wrapped);
    
    rb_eval_string("doit");
}

- (void) evalString:(NSString*)code {
    int err;
    rb_eval_string_protect([code UTF8String], &err);
    
    if (err) {
        VALUE exception = rb_gv_get("$!");
        VALUE excStr = rb_obj_as_string(exception);
        NSString* exceptionString = [NSString stringWithUTF8String:StringValueCStr(excStr)];
        NSLog(@"%@", exceptionString);
    }
}

@end
