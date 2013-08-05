//
//  SDConfigChooserWindowController.h
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#define SDRunMyScriptDefaultsKey @"runMyScript"
#define SDRunCommandFirstDefaultsKey @"runCommandFirst"
#define SDUseRelaunchPathsDefaultsKey @"useRelaunchPaths"

#define SDLaunchCommandDefaultsKey @"launchCommand"
#define SDPrerunCommandDefaultsKey @"prerunCommand"
#define SDRelaunchPathsDefaultsKey @"relaunchPaths"

#define SDScriptSocketTypeDefaultsKey @"scriptSocketType"
#define SDTCPSocketPortDefaultsKey @"tcpSocketPort"

@interface SDPreferencesWindowController : NSWindowController

+ (SDPreferencesWindowController*) sharedConfigChooserWindowController;

- (void) show;

@end
