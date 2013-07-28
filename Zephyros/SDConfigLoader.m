//
//  SDConfigLoader.m
//  Zephyros
//
//  Created by Steven on 4/15/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDConfigLoader.h"

#import <JSCocoa/JSCocoa.h>

#import "SDWindowProxy.h"
#import "SDScreenProxy.h"
#import "SDAPI.h"

#import "SDEventListener.h"
#import "SDKeyBinder.h"
#import "SDAlertWindowController.h"
#import "SDLogWindowController.h"

#import "SDConfigWatcher.h"


#import "SDJS.h"
#import "SDRuby.h"


@interface SDConfigLoader ()

@property SDConfigWatcher* configWatcher;

@property SDJS* js;
@property SDRuby* ruby;

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

- (void) prepareScriptingBridge {
    self.js = [[SDJS alloc] init];
    [self.js setup];
    
    self.ruby = [[SDRuby alloc] init];
    [self.ruby setup];
}

- (void) reloadConfigIfWatchEnabled {
    if ([[NSUserDefaults standardUserDefaults] boolForKey:@"AutoReloadConfigs"]) {
        // this (hopefully?) guards against there sometimes being 2 notifications in a row
        [[self class] cancelPreviousPerformRequestsWithTarget:self selector:@selector(reloadConfig) object:nil];
        [self performSelector:@selector(reloadConfig) withObject:nil afterDelay:0.1];
    }
}

- (void) reloadConfig {
    dispatch_async(dispatch_get_main_queue(), ^{
        [self.configWatcher stopWatching];
        
        NSString* file = [[NSUserDefaults standardUserDefaults] stringForKey:@"configPath"];
        
        if (!file) {
            [[SDAlertWindowController sharedAlertWindowController]
             show:[NSString stringWithFormat:@"Um, %@ doesn't seem to exist", file]
             delay:@7.0];
            return;
        }
        
        [[SDKeyBinder sharedKeyBinder] removeKeyBindings];
        [[SDEventListener sharedEventListener] removeListeners];
        
        if (![self load:file])
            return;
        
        [[SDEventListener sharedEventListener] finalizeNewListeners];
        
        NSArray* failures = [[SDKeyBinder sharedKeyBinder] finalizeNewKeyBindings];
        
        if ([failures count] > 0) {
            NSString* str = [@"The following hot keys could not be bound:\n\n" stringByAppendingString: [failures componentsJoinedByString:@"\n"]];
            [[SDLogWindowController sharedLogWindowController] show:str
                                                               type:SDLogMessageTypeError];
        }
        else {
            static BOOL loaded;
            [[SDAlertWindowController sharedAlertWindowController]
             show:[NSString stringWithFormat:@"%s %@", (loaded ? "Reloaded" : "Loaded"), file]
             delay:nil];
            loaded = YES;
        }
        
        [self.configWatcher startWatching:[[[NSUserDefaults standardUserDefaults] stringForKey:@"configPath"] stringByStandardizingPath]];
    });
}

- (BOOL) load:(NSString*)filename {
    NSString* contents = [NSString stringWithContentsOfFile:[filename stringByStandardizingPath]
                                                   encoding:NSUTF8StringEncoding
                                                      error:NULL];
    
    if (!contents)
        return NO;
    
    if ([[NSUserDefaults standardUserDefaults] boolForKey:@"configShouldPreprocess"]) {
        NSString* preprocessor = [[NSUserDefaults standardUserDefaults] stringForKey:@"configConverter"];
        if (preprocessor) {
            preprocessor = [preprocessor stringByStandardizingPath];
            NSDictionary* result = [SDAPI shell:@"/bin/bash"
                                           args:@[@"-lc", preprocessor]
                                        options:@{@"input": contents, @"pwd":[preprocessor stringByDeletingLastPathComponent]}];
            contents = [result objectForKey:@"stdout"];
        }
    }
    
    NSString* type = [[NSUserDefaults standardUserDefaults] stringForKey:@"configType"];
    
    if ([type isEqualToString: @"js"]) {
        [self.js evalString:contents asCoffee:NO];
    }
    else if ([type isEqualToString: @"coffee"]) {
        [self.js evalString:contents asCoffee:YES];
    }
    else if ([type isEqualToString: @"ruby"]) {
        [self.ruby evalString:contents];
    }
    
    return YES;
}

@end
