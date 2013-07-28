//
//  SDRuby.m
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDRuby.h"


#include <ruby/ruby.h>



VALUE sd_bind_keys_fn(VALUE module, VALUE keys, VALUE callback) {
//    NSString* ss = [NSString stringWithUTF8String:StringValueCStr(keys)];
//    
//    NSLog(@"it was [%@]", ss);
//    
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
    rb_define_module_function(api_module, "bind", sd_bind_keys_fn, 2);
    
//    rb_require("sum");
//    return ruby_cleanup(0);
}

- (void) evalString:(NSString*)code {
    
//    rb_protect;
    rb_eval_string([code UTF8String]);
}

@end
