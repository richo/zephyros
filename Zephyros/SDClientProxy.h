//
//  SDClientProxy.h
//  Zephyros
//
//  Created by Steven Degutis on 8/12/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "SDClient.h"

@interface SDClientProxy : NSObject

@property (weak) SDClient* client;
@property id receiver;

@end




#define SDTypeCheckArg(klass, name, idx) \
klass* name; \
do { \
id arg = [args objectAtIndex:idx]; \
if (![arg isKindOfClass:[klass self]]) { \
[self.client showAPIError:[NSString stringWithFormat:@"API Error: in method [%@] on object of type [%@], argument %d was expected to be type %@ but was %@", \
NSStringFromSelector(_cmd), \
[self className], \
idx, \
[klass self], \
[arg className]]]; \
return nil; \
} \
name = arg; \
} while(0) \
