//
//  SDRuby.m
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDRuby.h"


#import <objc/runtime.h>

#import "SDRubyObject.h"


#import "SDAPI.h"
#import "SDKeyBinder.h"



VALUE SDWrappedObject(id thing) {
    NSString* className = NSStringFromClass([thing class]);
    
    if ([className hasPrefix:@"SD"])
        className = [className substringFromIndex:2];
    else
        className = @"WrappedObject";
    
    return Data_Wrap_Struct(rb_eval_string([className UTF8String]), NULL, NULL, (__bridge void*)thing);
}





VALUE SDObjcToRubyValue(id obj) {
    if (obj == nil || [obj isEqual:[NSNull null]]) {
        return Qnil;
    }
    if ([obj isKindOfClass:[NSNumber self]]) {
        if (strcmp([obj objCType], "i") == 0) {
            return LONG2FIX([obj longValue]);
        }
        if (strcmp([obj objCType], "d") == 0) {
            return rb_float_new([obj doubleValue]);
        }
        if (strcmp([obj objCType], "c") == 0) {
            return ([obj boolValue] ? Qtrue : Qfalse);
        }
    }
    if ([obj isKindOfClass:[NSString self]]) {
        return rb_str_new2([obj UTF8String]);
    }
    if ([obj isKindOfClass:[NSArray self]]) {
        VALUE array = rb_ary_new();
        for (id element in obj) {
            rb_ary_push(array, SDObjcToRubyValue(element));
        }
        return array;
    }
    if ([obj isKindOfClass:[NSDictionary self]]) {
        VALUE hash = rb_hash_new();
        for (id key in obj) {
            id val = [obj objectForKey:key];
            rb_hash_aset(hash, SDObjcToRubyValue(key), SDObjcToRubyValue(val));
        }
        return hash;
    }
    
    return SDWrappedObject(obj);
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
    id internalObj = (__bridge id)s;
    
    NSMutableArray* objsArgs = SDRubyToObjcValue(args);
    
    if (has_block)
        [objsArgs addObject:SDRubyToObjcValue(given_block)];
    
    NSString* originalSelStr = [objsArgs objectAtIndex:0];
    NSString* selStr = [originalSelStr stringByReplacingOccurrencesOfString:@"_" withString:@":"];
    SEL sel = NSSelectorFromString(selStr);
    
    [objsArgs removeObjectAtIndex:0];
    
    NSMethodSignature* sig = [internalObj methodSignatureForSelector:sel];
    
    if (!sig) {
        rb_raise(rb_eTypeError, [[NSString stringWithFormat:@"undefined ObjC method = %@", originalSelStr] UTF8String]);
    }
    
    NSInvocation* inv = [NSInvocation invocationWithMethodSignature:sig];
    
    [inv setSelector:sel];
    [inv setTarget:internalObj];
    
    int i = 0;
    for (id arg in objsArgs) {
        id temparg = arg;
        [inv setArgument:&temparg atIndex:i+2];
        i++;
    }
    
    [inv invoke];
    
    if (strcmp([sig methodReturnType], "@") == 0) {
        __unsafe_unretained id result;
        [inv getReturnValue:&result];
        id strongResult = result; // maybe not necessary but im paranoid these days
        VALUE val = SDObjcToRubyValue(strongResult);
        return val;
    }
    
    return Qnil;
}



@implementation SDRuby

- (void) setup {
    RUBY_INIT_STACK;
    ruby_init();
    ruby_init_loadpath();
    
    VALUE c = rb_define_class("WrappedObject", rb_cObject);
    rb_define_method(c, "method_missing", RUBY_METHOD_FUNC(sd_method_missing), -2);
    
    for (NSString* name in @[@"Window", @"Screen", @"App", @"API", @"KeyBinder"]) {
        rb_define_class([name UTF8String], rb_eval_string("WrappedObject"));
    }
    
    rb_gv_set("api", SDWrappedObject([SDAPI self]));
    rb_gv_set("keybinder", SDWrappedObject([SDKeyBinder sharedKeyBinder]));
    
    rb_require([[[NSBundle mainBundle] pathForResource:@"api" ofType:@"rb"] UTF8String]);
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
