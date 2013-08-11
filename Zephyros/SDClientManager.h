//
//  SDClientManager.h
//  Zephyros
//
//  Created by Steven on 8/10/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

//@protocol SDClientDelegate <NSObject>
//
//- (void) clientKickedTheBucket:(id)client;
//
//@end

@interface SDClientManager : NSObject

+ (SDClientManager*) sharedClientManager;

@end
