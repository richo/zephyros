//
//  SDClientProxy.m
//  Zephyros
//
//  Created by Steven Degutis on 8/12/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDClientProxy.h"

#import "SDLogWindowController.h"

@implementation SDClientProxy

- (id) call:(NSString*)meth args:(NSArray*)args msgID:(id)msgID {
    SEL sel = NSSelectorFromString([[meth stringByReplacingOccurrencesOfString:@"?" withString:@"_q"] stringByAppendingString:@":msgID:"]);
    
    if (![self respondsToSelector:sel]) {
        NSString* error = [NSString stringWithFormat:@"API Error: Could not find method [%@] on object of type [%@]", meth, [self className]];
        [[SDLogWindowController sharedLogWindowController] show:error
                                                           type:SDLogMessageTypeError];
        
        return nil;
    }
    
#pragma clang diagnostic push // in' as you're shovin', and I'm slippin' back into the gap again
#pragma clang diagnostic ignored "-Warc-performSelector-leaks" // *plonk*
    return [self performSelector:sel withObject:args withObject:msgID];
#pragma clang diagnostic pop // rocks aren't all they're cracked up to be
}

//- (void) check:(NSArray*)args forTypes:(NSArray*)types inMethod:(SEL)method {
//    int i = 0;
//    for (Class klass in types) {
//        id arg = [args objectAtIndex:i];
//        if (![arg isKindOfClass:klass]) {
//            NSString* error = [NSString stringWithFormat:@"API Error: in method [%@] on object of type [%@], argument %d was expected to be type %@ but was %@", NSStringFromSelector(method), [self className], i, klass, [arg className]];
//            [[SDLogWindowController sharedLogWindowController] show:error
//                                                               type:SDLogMessageTypeError];
//            
//            return nil;
//        }
//        
//        i++;
//    }
//}

@end
