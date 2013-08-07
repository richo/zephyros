//
//  MyWindow.h
//  Zephyros
//
//  Created by Steven Degutis on 2/28/13.
//  Copyright (c) 2013 Steven Degutis. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "SDAppProxy.h"
#import "SDScreenProxy.h"

@interface SDWindowProxy : NSObject

- (id) initWithElement:(AXUIElementRef)win;

// getting windows

+ (NSArray*) allWindows;
+ (NSArray*) visibleWindows;
+ (SDWindowProxy*) focusedWindow;
- (NSArray*) otherWindowsOnSameScreen;
- (NSArray*) otherWindowsOnAllScreens;


// window position & size

- (NSDictionary*) frame;
- (void) setFrame:(NSDictionary*)frame;

- (void) setTopLeft:(NSDictionary*)thePoint;
- (void) setSize:(NSDictionary*)theSize;

- (NSDictionary*) topLeft;
- (NSDictionary*) size;

- (void) maximize;
- (void) minimize;
- (void) unMinimize;


// other

- (SDScreenProxy*) screen;
- (SDAppProxy*) app;

- (NSNumber*) isNormalWindow;

// focus

- (NSNumber*) focusWindow;

- (void) focusWindowLeft;
- (void) focusWindowRight;
- (void) focusWindowUp;
- (void) focusWindowDown;

- (NSArray*) windowsToWest;
- (NSArray*) windowsToEast;
- (NSArray*) windowsToNorth;
- (NSArray*) windowsToSouth;


// other window properties

- (NSString *) title;
- (NSNumber*) isWindowMinimized;

@end
