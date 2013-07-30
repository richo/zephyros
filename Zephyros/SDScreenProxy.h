//
//  NSScreenProxy.h
//  Zephyros
//
//  Created by Steven on 4/14/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "SDGeometry.h"

@interface SDScreenProxy : NSObject

@property NSScreen* actualScreenObject;

+ (SDScreenProxy*) mainScreen;
+ (NSArray*) allScreens;

- (SDRect*) frameIncludingDockAndMenu;
- (SDRect*) frameWithoutDockOrMenu;

- (SDScreenProxy*) nextScreen;
- (SDScreenProxy*) previousScreen;

@end
