//
//  SDConfigChooserWindowController.h
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface SDConfigChooserWindowController : NSWindowController

+ (SDConfigChooserWindowController*) sharedConfigChooserWindowController;

- (void) show;

@end
