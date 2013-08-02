//
//  SDConfigLoader.m
//  Zephyros
//
//  Created by Steven on 4/15/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDConfigLoader.h"

#import "SDPreferencesWindowController.h"
#import "SDConfigWatcher.h"


@interface SDConfigLoader ()

@property SDConfigWatcher* configWatcher;
@property NSTask* launchedTask;

@end


@implementation SDConfigLoader

+ (SDConfigLoader*) sharedConfigLoader {
    static SDConfigLoader* sharedConfigLoader;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedConfigLoader = [[SDConfigLoader alloc] init];
        sharedConfigLoader.configWatcher = [[SDConfigWatcher alloc] init];
    });
    return sharedConfigLoader;
}

- (void) launchConfigMaybe {
    dispatch_async(dispatch_get_main_queue(), ^{
        BOOL shouldLaunchConfig = [[NSUserDefaults standardUserDefaults] boolForKey:SDRunMyScriptDefaultsKey];
        
        if (shouldLaunchConfig) {
            if ([self isLaunched])
                [self unlaunch];
            
            [self launch];
        }
        else {
            [self unlaunch];
            [self.configWatcher stopWatching];
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
        [self.configWatcher startWatching:paths];
    }
    else {
        [self.configWatcher stopWatching];
    }
}




- (void) launch {
    NSString* cmd = [[NSUserDefaults standardUserDefaults] stringForKey:SDLaunchCommandDefaultsKey];
    self.launchedTask = [NSTask launchedTaskWithLaunchPath:@"/bin/bash" arguments:@[@"-l", @"-c", cmd]];
}

- (void) unlaunch {
    [self.launchedTask terminate];
    self.launchedTask = nil;
}

- (BOOL) isLaunched {
    return self.launchedTask != nil;
}


//            NSString* str = [@"The following hot keys could not be bound:\n\n" stringByAppendingString: [failures componentsJoinedByString:@"\n"]];
//            [[SDLogWindowController sharedLogWindowController] show:str
//                                                               type:SDLogMessageTypeError];



//            [[SDAlertWindowController sharedAlertWindowController]
//             show:@"Launched Config"
//             delay:nil];



//- (id) eval:(NSString*)str {
//    if ([[NSUserDefaults standardUserDefaults] boolForKey:@"configShouldPreprocess"]) {
//        NSString* preprocessor = [[NSUserDefaults standardUserDefaults] stringForKey:@"configConverter"];
//        if (preprocessor) {
//            preprocessor = [preprocessor stringByStandardizingPath];
//            NSDictionary* result = [SDAPI shell:@"/bin/bash"
//                                           args:@[@"-lc", preprocessor]
//                                        options:@{@"input": str, @"pwd":[preprocessor stringByDeletingLastPathComponent]}];
//            str = [result objectForKey:@"stdout"];
//        }
//    }


@end
