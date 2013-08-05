//
//  SDConfigChooserWindowController.m
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDPreferencesWindowController.h"

#import "SDConfigLauncher.h"

#import "SDWatchedPathsWindowController.h"

#import "SDClientListener.h"

@interface SDPreferencesWindowController ()

@property SDWatchedPathsWindowController* pathsController;

@end

@implementation SDPreferencesWindowController

+ (SDPreferencesWindowController*) sharedConfigChooserWindowController {
    static SDPreferencesWindowController* sharedConfigChooserWindowController;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedConfigChooserWindowController = [[SDPreferencesWindowController alloc] init];
    });
    return sharedConfigChooserWindowController;
}

- (NSString*) windowNibName {
    return @"SDPreferencesWindow";
}

- (void) show {
    [[self window] center];
    [self showWindow:self];
}

- (IBAction) changeIfRunsScript:(id)sender {
    [[SDConfigLauncher sharedConfigLauncher] launchConfigMaybe];
}

- (IBAction) changeWhetherWatchingPaths:(id)sender {
    [[SDConfigLauncher sharedConfigLauncher] watchPathsMaybe];
}

- (IBAction) switchSocketType:(id)sender {
    [[SDClientListener sharedListener] startListening];
}

- (IBAction) switchSocketPort:(id)sender {
    [[SDClientListener sharedListener] startListening];
}

- (BOOL)control:(NSControl *)control textView:(NSTextView *)textView doCommandBySelector:(SEL)command {
    if (command == @selector(insertNewline:)) {
        [textView insertNewlineIgnoringFieldEditor:self];
        return YES;
    }
    return NO;
}

- (IBAction) changePaths:(id)sender {
    self.pathsController = [[SDWatchedPathsWindowController alloc] init];
    [NSApp beginSheet:[self.pathsController window]
       modalForWindow:[self window]
        modalDelegate:self
       didEndSelector:@selector(pathsSheetDidEnd:returnCode:contextInfo:)
          contextInfo:NULL];
}

- (void) pathsSheetDidEnd:(NSWindow *)sheet returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo {
    [sheet orderOut:self];
    
    if (returnCode) {
        [[NSUserDefaults standardUserDefaults] setObject:self.pathsController.pathsToWatch
                                                  forKey:SDRelaunchPathsDefaultsKey];
        
        [[SDConfigLauncher sharedConfigLauncher] watchPathsMaybe];
    }
    
    self.pathsController = nil;
}

@end
