//
//  SDConfigLoader.m
//  Zephyros
//
//  Created by Steven on 4/15/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDConfigLauncher.h"

#import "SDPreferencesWindowController.h"
#import "SDPathWatcher.h"

#import "SDLogWindowController.h"
#import "SDAlertWindowController.h"

#import "SDShellCommand.h"

@interface SDConfigLauncher ()

@property SDPathWatcher* configWatcher;
@property SDShellCommand* launchedTask;

@end


@implementation SDConfigLauncher

+ (SDConfigLauncher*) sharedConfigLauncher {
    static SDConfigLauncher* sharedConfigLauncher;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedConfigLauncher = [[SDConfigLauncher alloc] init];
        sharedConfigLauncher.configWatcher = [[SDPathWatcher alloc] init];
    });
    return sharedConfigLauncher;
}

- (void) launchConfigMaybe {
    dispatch_async(dispatch_get_main_queue(), ^{
        NSString* cmd = [[NSUserDefaults standardUserDefaults] stringForKey:SDLaunchCommandDefaultsKey];
        BOOL shouldLaunchConfig = [[NSUserDefaults standardUserDefaults] boolForKey:SDRunMyScriptDefaultsKey] && [cmd length] > 0;
        
        if (shouldLaunchConfig) {
            if ([self isLaunched])
                [self unlaunch];
            
            [self launch];
        }
        else {
            [self unlaunch];
            self.configWatcher = nil;
        }
        
        [self watchPathsMaybe];
    });
}

- (void) watchPathsMaybe {
    BOOL shouldLaunchConfig = [[NSUserDefaults standardUserDefaults] boolForKey:SDRunMyScriptDefaultsKey];
    BOOL shouldWatchPaths = [[NSUserDefaults standardUserDefaults] boolForKey:SDUseRelaunchPathsDefaultsKey];
    
    if (shouldLaunchConfig && shouldWatchPaths) {
        NSString* pathsStr = [[NSUserDefaults standardUserDefaults] stringForKey:SDRelaunchPathsDefaultsKey];
        NSArray* paths = [pathsStr componentsSeparatedByString:@"\n"];
        self.configWatcher = [SDPathWatcher watcherFor:paths];
    }
    else {
        self.configWatcher = nil;
    }
}




- (void) prelaunchMaybe {
    BOOL shouldPrelaunch = [[NSUserDefaults standardUserDefaults] boolForKey:SDRunCommandFirstDefaultsKey];
    
    if (!shouldPrelaunch)
        return;
    
    NSString* prelaunchCmd = [[NSUserDefaults standardUserDefaults] stringForKey:SDPrerunCommandDefaultsKey];
    
    NSTask* task = [NSTask launchedTaskWithLaunchPath:@"/bin/bash" arguments:@[@"-l", @"-c", prelaunchCmd]];
    [task waitUntilExit];
}

- (void) launch {
    [self prelaunchMaybe];
    
    NSString* cmd = [[NSUserDefaults standardUserDefaults] stringForKey:SDLaunchCommandDefaultsKey];
    
    self.launchedTask = [[SDShellCommand alloc] init];
    self.launchedTask.cmd = cmd;
    self.launchedTask.gotStdout = ^(NSFileHandle* handle) {
        NSData* data = [handle availableData];
        NSString* str = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        
        dispatch_async(dispatch_get_main_queue(), ^{
            [[SDLogWindowController sharedLogWindowController] show:str
                                                               type:SDLogMessageTypeUser];
        });
    };
    self.launchedTask.gotStderr = ^(NSFileHandle* handle) {
        NSData* data = [handle availableData];
        NSString* str = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        
        dispatch_async(dispatch_get_main_queue(), ^{
            [[SDLogWindowController sharedLogWindowController] show:str
                                                               type:SDLogMessageTypeError];
        });
    };
    
    __weak SDConfigLauncher* punyself = self;
    self.launchedTask.died = ^{
        [[NSNotificationCenter defaultCenter] postNotificationName:SDScriptDiedNotification object:nil];
        punyself.launchedTask = nil;
    };
    
    [self.launchedTask launch];
    
    [[NSNotificationCenter defaultCenter] postNotificationName:SDScriptLaunchedNotification object:nil];
}

- (void) unlaunch {
    [self.launchedTask kill];
    self.launchedTask = nil;
}

- (BOOL) isLaunched {
    return self.launchedTask != nil;
}

@end
