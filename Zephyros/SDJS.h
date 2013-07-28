//
//  SDJS.h
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SDJS : NSObject

- (void) setup;

- (NSString*) evalString:(NSString*)str asCoffee:(BOOL)useCoffee;

@end
