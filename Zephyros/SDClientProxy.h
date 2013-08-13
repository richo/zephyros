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

- (id) call:(NSString*)meth args:(NSArray*)args msgID:(id)msgID;
//- (void) check:(NSArray*)args forTypes:(NSArray*)types inMethod:(SEL)method;

@end
