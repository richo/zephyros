//
//  NSScreenProxy.h
//  Zephyros
//
//  Created by Steven on 4/14/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SDScreenProxy : NSObject

@property (strong) NSScreen* actualScreenObject;

+ (SDScreenProxy*) mainScreen;
+ (NSArray*) allScreens;

- (NSDictionary*) frameIncludingDockAndMenu;
- (NSDictionary*) frameWithoutDockOrMenu;

- (SDScreenProxy*) nextScreen;
- (SDScreenProxy*) previousScreen;

@end
