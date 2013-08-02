//
//  SDConfigChooserWindowController.m
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDPreferencesWindowController.h"

#import "SDConfigLoader.h"

#import "SDWatchedPathsWindowController.h"

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
    [[SDConfigLoader sharedConfigLoader] launchConfigMaybe];
}

- (IBAction) changeWhetherWatchingPaths:(id)sender {
    [[SDConfigLoader sharedConfigLoader] watchPathsMaybe];
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
    }
    
    self.pathsController = nil;
}

@end
