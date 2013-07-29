//
//  SDRuby.m
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDRuby.h"


#include <ruby/ruby.h>



VALUE sd_bind_keys_fn(VALUE module, VALUE keys) {
    VALUE p = rb_block_proc();
    
    rb_funcall(p, rb_intern("call"), 0);
    
    NSString* ss = [NSString stringWithUTF8String:StringValueCStr(keys)];
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
    
    VALUE api_module = rb_define_module("API");
    rb_define_module_function(api_module, "bind", sd_bind_keys_fn, 1);
    
    rb_require([[[NSBundle mainBundle] pathForResource:@"api" ofType:@"rb"] UTF8String]);
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
