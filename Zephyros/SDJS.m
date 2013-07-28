//
//  SDJS.m
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDJS.h"

#import <JSCocoa/JSCocoa.h>

#import "SDAlertWindowController.h"
#import "SDLogWindowController.h"

@interface SDJS ()

@property JSCocoa* jscocoa;

@end

@implementation SDJS

- (void) setup {
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

- (void) evalCoffeeFile:(NSString*)path {
    NSString* contents = [NSString stringWithContentsOfFile:[path stringByStandardizingPath]
                                                   encoding:NSUTF8StringEncoding
                                                      error:NULL];
    [self evalString:contents asCoffee:YES];
}

// yes, its dupliation. but we still need this for require() to work in people's own configs.
// consider it a bit of legacy :(
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
