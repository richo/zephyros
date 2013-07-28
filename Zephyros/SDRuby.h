//
//  SDRuby.h
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SDRuby : NSObject

- (void) setup;

- (void) evalString:(NSString*)code;

@end
