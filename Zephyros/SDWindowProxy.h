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
#import "SDGeometry.h"

@interface SDWindowProxy : NSObject

- (id) initWithElement:(AXUIElementRef)win;

// getting windows

+ (NSArray*) allWindows;
+ (NSArray*) visibleWindows;
+ (SDWindowProxy*) focusedWindow;
- (NSArray*) otherWindowsOnSameScreen;


// window position & size

- (SDRect*) frame;
- (void) setFrame:(SDRect*)frame;

- (void) setTopLeft:(SDPoint*)thePoint;
- (void) setSize:(SDSize*)theSize;

- (SDPoint*) topLeft;
- (SDSize*) size;

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


// other window properties

- (NSString *) title;
- (NSNumber*) isWindowMinimized;

@end
