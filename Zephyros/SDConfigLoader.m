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

@interface SDConfigLoader ()

@property SDConfigWatcher* configWatcher;

@property JSCocoa* jscocoa;

- (void) reloadConfigIfWatchEnabled;

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
    self.jscocoa = [JSCocoa new];
    self.jscocoa.delegate = self;
    self.jscocoa.useAutoCall = YES;
    self.jscocoa.useSplitCall = NO;
    self.jscocoa.useJSLint = NO;
    self.jscocoa.useAutoCall = NO;
    
    [self.jscocoa evalJSFile:[[NSBundle mainBundle] pathForResource:@"underscore-min" ofType:@"js"]];
    [self.jscocoa evalJSFile:[[NSBundle mainBundle] pathForResource:@"coffee-script" ofType:@"js"]];
    [self.jscocoa eval:@"function coffeeToJS(coffee) { return CoffeeScript.compile(coffee, { bare: true }); };"];
    [self evalCoffeeFile:[[NSBundle mainBundle] pathForResource:@"api" ofType:@"coffee"]];
}

- (void) reloadConfigIfWatchEnabled {
    if ([[NSUserDefaults standardUserDefaults] boolForKey:@"AutoReloadConfigs"]) {
        // this (hopefully?) guards against there sometimes being 2 notifications in a row
        [[self class] cancelPreviousPerformRequestsWithTarget:self selector:@selector(reloadConfig) object:nil];
        [self performSelector:@selector(reloadConfig) withObject:nil afterDelay:0.1];
    }
}

- (void) evalCoffeeFile:(NSString*)path {
    NSString* contents = [NSString stringWithContentsOfFile:[path stringByStandardizingPath]
                                                   encoding:NSUTF8StringEncoding
                                                      error:NULL];
    [self evalString:contents asCoffee:YES];
}

- (void) reloadConfig {
    dispatch_async(dispatch_get_main_queue(), ^{
        [self.configWatcher stopWatching];
        
        NSString* file = [[[NSUserDefaults standardUserDefaults] stringForKey:@"configPath"] stringByStandardizingPath];
        
        if (!file) {
            [[SDAlertWindowController sharedAlertWindowController]
             show:@"Can't find either ~/.zephyros.{coffee,js}\n\nMake one exist and try Reload Config again."
             delay:@7.0];
            return;
        }
        
        [[SDKeyBinder sharedKeyBinder] removeKeyBindings];
        [[SDEventListener sharedEventListener] removeListeners];
        
        if (![self require:file])
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

- (BOOL) require:(NSString*)filename {
    if ([filename isAbsolutePath] == NO)
        filename = [@"~/.zephyros/" stringByAppendingPathComponent:filename];
    
    NSString* contents = [NSString stringWithContentsOfFile:[filename stringByStandardizingPath]
                                                   encoding:NSUTF8StringEncoding
                                                      error:NULL];
    
    NSLog(@"ok %@", contents);
    
    if (!contents)
        return NO;
    
    if ([filename hasSuffix:@".js"]) {
        [self evalString:contents asCoffee:NO];
        return YES;
    }
    
    if ([filename hasSuffix:@".coffee"]) {
        [self evalString:contents asCoffee:YES];
        return YES;
    }
    
//    NSString* suffix = [filename pathExtension];
//    NSString* compiler = [[nil objectForKey:suffix] stringByStandardizingPath];
//    if (compiler) {
//        NSDictionary* result = [SDAPI shell:@"/bin/bash"
//                                       args:@[@"-lc", compiler]
//                                    options:@{@"input": contents, @"pwd":[compiler stringByDeletingLastPathComponent]}];
//        NSString* output = [result objectForKey:@"stdout"];
//        [self.jscocoa eval:output];
//        return YES;
//    }
    
    [[SDAlertWindowController sharedAlertWindowController]
     show:[NSString stringWithFormat:@"Don't know how to load %@", filename]
     delay:@4.0];
    return NO;
}

- (NSString*) evalString:(NSString*)str asCoffee:(BOOL)useCoffee {
    if (useCoffee)
        return [self evalString:[self.jscocoa callFunction:@"coffeeToJS" withArguments:@[str]]
                       asCoffee:NO];
    else
        return [[self.jscocoa eval:str] description];
}

- (void) JSCocoa:(JSCocoaController*)controller hadError:(NSString*)error onLineNumber:(NSInteger)lineNumber atSourceURL:(id)url {
    NSString* msg = [NSString stringWithFormat: @"Error in config file on line: %ld\n\n%@", lineNumber, error];
    [[SDLogWindowController sharedLogWindowController] show:msg type:SDLogMessageTypeError];
}

@end
