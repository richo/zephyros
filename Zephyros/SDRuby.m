//
//  SDRuby.m
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDRuby.h"


#import "SDAPI.h"

#import <objc/runtime.h>
#include <ruby/ruby.h>

#import "SDRubyObject.h"



VALUE SDObjcToRubyValue(id obj) {
    return Qnil;
}


id SDRubyToObjcValue(VALUE obj) {
    switch (TYPE(obj)) {
        case T_FIXNUM:
            return @(FIX2LONG(obj));
        case T_FLOAT:
            return @(NUM2DBL(obj));
        case T_STRING:
            return [NSString stringWithUTF8String:StringValueCStr(obj)];
            break;
        case T_SYMBOL:
            return [NSString stringWithUTF8String:rb_id2name(SYM2ID(obj))];
            break;
        case T_TRUE:
            return @YES;
        case T_FALSE:
            return @NO;
        case T_NIL:
            return [NSNull null];
        case T_HASH:
        {
            VALUE array_of_kv_tuples = rb_funcall(obj, rb_intern("to_a"), 0);
            
            NSMutableDictionary* dict = [NSMutableDictionary dictionary];
            VALUE* ptr = RARRAY_PTR(array_of_kv_tuples);
            
            for (int i = 0; i < RARRAY_LEN(array_of_kv_tuples); i++) {
                VALUE kv_tuple = ptr[i];
                VALUE* tuple_ptr = RARRAY_PTR(kv_tuple);
                
                [dict setObject:SDRubyToObjcValue(tuple_ptr[1])
                         forKey:SDRubyToObjcValue(tuple_ptr[0])];
            }
            
            return dict;
        }
        case T_ARRAY:
        {
            NSMutableArray* array = [NSMutableArray array];
            VALUE* ptr = RARRAY_PTR(obj);
            
            for (int i = 0; i < RARRAY_LEN(obj); i++) {
                VALUE v = ptr[i];
                id o = SDRubyToObjcValue(v);
                [array addObject:o];
            }
            
            return array;
        }
        case T_DATA:
            return [SDRubyObject withRubyValue:obj];
        default:
            rb_raise(rb_eTypeError, [[NSString stringWithFormat:@"not valid value, class = %@", SDRubyToObjcValue(rb_funcall(CLASS_OF(obj), rb_intern("name"), 0))] UTF8String]);
            break;
    }
    
    return nil;
}



VALUE sd_method_missing(VALUE self, VALUE args) {
    BOOL has_block = NO;
    VALUE given_block;
    if (rb_block_given_p()) {
        given_block = rb_block_proc();
        has_block = YES;
    }
    
    void* s;
    Data_Get_Struct(self, void, s);
    id internalObj = (__bridge_transfer id)s;
    
    NSMutableArray* objsArgs = SDRubyToObjcValue(args);
    
    if (has_block)
        [objsArgs addObject:SDRubyToObjcValue(given_block)];
    
    NSString* selStr = [objsArgs objectAtIndex:0];
    selStr = [selStr stringByReplacingOccurrencesOfString:@"_" withString:@":"];
    SEL sel = NSSelectorFromString(selStr);
    
    [objsArgs removeObjectAtIndex:0];
    
    NSInvocation* inv = [NSInvocation invocationWithMethodSignature:[internalObj methodSignatureForSelector:sel]];
    
    [inv setSelector:sel];
    [inv setTarget:internalObj];
    
    int i = 0;
    for (id arg in objsArgs) {
        id temparg = arg;
        [inv setArgument:&temparg atIndex:i+2];
        i++;
    }
    
    [inv invoke];
    
    id result;
//    [inv getReturnValue:&result];
    
    return SDObjcToRubyValue(result);
}



VALUE SDWrappedObject(id thing) {
    NSString* className = NSStringFromClass([thing class]);
    
    if (![className hasPrefix:@"SD"])
        className = @"WrappedObject";
    
    return Data_Wrap_Struct(rb_eval_string([className UTF8String]), NULL, NULL, (__bridge void*)thing);
}


@implementation SDRuby

- (void) setup {
    RUBY_INIT_STACK;
    ruby_init();
    ruby_init_loadpath();
    
    VALUE c = rb_define_class("WrappedObject", rb_cObject);
    rb_define_method(c, "method_missing", RUBY_METHOD_FUNC(sd_method_missing), -2);
    
    rb_require([[[NSBundle mainBundle] pathForResource:@"api" ofType:@"rb"] UTF8String]);
    
    
    
    
    
    
    rb_gv_set("internal", SDWrappedObject([SDAPI self]));
    
    rb_eval_string("after 2 do puts 'ok' end");
    
//    rb_eval_string("@something.woo(true, false, lambda{ puts 'whoa' }, nil, :hi, {:a => 2, :b => [:hi]}) { puts 'hi' }");
}

- (void) evalString:(NSString*)code {
    int err;
    rb_eval_string_protect([code UTF8String], &err);
    
    if (err) {
        VALUE exception = rb_gv_get("$!");
        VALUE excStr = rb_obj_as_string(exception);
        NSString* exceptionString = [NSString stringWithUTF8String:StringValueCStr(excStr)];
        
        // TODO: show exceptions via log window
        NSLog(@"%@", exceptionString);
    }
}

@end
