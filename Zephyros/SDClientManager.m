//
//  SDClientManager.m
//  Zephyros
//
//  Created by Steven on 8/10/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDClientManager.h"

@implementation SDClientManager

+ (SDClientManager*) sharedClientManager {
    static SDClientManager* sharedClientManager;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedClientManager = [[SDClientManager alloc] init];
    });
    return sharedClientManager;
}

@end
