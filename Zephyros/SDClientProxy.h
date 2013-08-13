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

@property SDClient* client;
@property NSNumber* msgID;
@property id receiver;

@end
