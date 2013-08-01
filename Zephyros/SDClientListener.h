//
//  SDClientListener.h
//  Zephyros
//
//  Created by Steven Degutis on 7/31/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SDClientListener : NSObject

+ (SDClientListener*) sharedListener;

- (void) startListening;

@end
