//
//  AppDelegate.m
//  Zephyros
//
//  Created by Steven on 4/13/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "AppDelegate.h"

#import "SDOpenAtLogin.h"
#import "SDConfigLauncher.h"
#import "SDAppStalker.h"

#import "SDLogWindowController.h"
#import "SDPreferencesWindowController.h"

#import "SDConfigLauncher.h"

#import "SDAlertWindowController.h"
#import "SDClientListener.h"


@interface AppDelegate ()

@property NSStatusItem* statusItem;

@end

@implementation AppDelegate

- (void) prepareStatusItem {
    [[NSNotificationCenter defaultCenter] addObserverForName:SDScriptLaunchedNotification
                                                      object:nil
                                                       queue:nil
                                                  usingBlock:^(NSNotification *note) {
                                                      static BOOL firstTime = YES;
                                                      if (firstTime) {
                                                          firstTime = NO;
                                                      }
                                                      else {
                                                          [[SDAlertWindowController sharedAlertWindowController] show:@"Launched Zephyros Script"
                                                                                                                delay:nil];
                                                      }
                                                  }];
    
    [[NSNotificationCenter defaultCenter] addObserverForName:SDScriptDiedNotification
                                                      object:nil
                                                       queue:nil
                                                  usingBlock:^(NSNotification *note) {
                                                      [[SDAlertWindowController sharedAlertWindowController] show:@"Zephyros Script Ended"
                                                                                                            delay:nil];
                                                  }];
    
    [[SDClientListener sharedListener] startListening];
    
    self.statusItem = [[NSStatusBar systemStatusBar] statusItemWithLength:NSVariableStatusItemLength];
    self.statusItem.image = [NSImage imageNamed:@"statusitem"];
    self.statusItem.alternateImage = [NSImage imageNamed:@"statusitem_pressed"];
    self.statusItem.menu = self.statusItemMenu;
    self.statusItem.highlightMode = YES;
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
    [self prepareStatusItem];
    
    [[SDConfigLauncher sharedConfigLauncher] launchConfigMaybe];
    [[SDAppStalker sharedAppStalker] beginStalking];
    
    [[SDAlertWindowController sharedAlertWindowController] show:@"Zephyros power, activate!"
                                                          delay:@1.5];
}

- (IBAction) showPreferencesWindow:(id)sender {
    [NSApp activateIgnoringOtherApps:YES];
    [[SDPreferencesWindowController sharedConfigChooserWindowController] show];
}

- (IBAction) relaunchConfig:(id)sender {
    [[SDConfigLauncher sharedConfigLauncher] launchConfigMaybe];
}

- (void) menuNeedsUpdate:(NSMenu *)menu {
    [[menu itemWithTitle:@"Open at Login"] setState:([SDOpenAtLogin opensAtLogin] ? NSOnState : NSOffState)];
}

- (IBAction) showLogWindow:(id)sender {
    [NSApp activateIgnoringOtherApps:YES];
    [[SDLogWindowController sharedLogWindowController] showWindow:self];
}

- (IBAction) showAboutPanel:(id)sender {
    [NSApp activateIgnoringOtherApps:YES];
    [NSApp orderFrontStandardAboutPanel:sender];
}

- (IBAction) toggleOpensAtLogin:(id)sender {
	NSInteger changingToState = ![sender state];
	[SDOpenAtLogin setOpensAtLogin: changingToState];
}

@end
