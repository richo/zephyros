//
//  SDConfigChooserWindowController.m
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDConfigChooserWindowController.h"

#import "SDConfigLoader.h"

@interface SDConfigChooserWindowController ()

@property NSString* maybeConfigName;
@property NSString* maybeConfigType;
@property NSString* maybeConfigConverter;
@property BOOL configShouldConvertFirst;

@property (readonly) CGFloat configWindowHeight;

@end

@implementation SDConfigChooserWindowController

+ (SDConfigChooserWindowController*) sharedConfigChooserWindowController {
    static SDConfigChooserWindowController* sharedConfigChooserWindowController;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedConfigChooserWindowController = [[SDConfigChooserWindowController alloc] init];
    });
    return sharedConfigChooserWindowController;
}

- (NSString*) windowNibName {
    return @"SDConfigChooserWindow";
}

- (IBAction) okayYeahUseThisFile:(id)sender {
    [[NSUserDefaults standardUserDefaults] setObject:self.maybeConfigName forKey:@"configPath"];
    [[NSUserDefaults standardUserDefaults] setObject:self.maybeConfigConverter forKey:@"configConverter"];
    [[NSUserDefaults standardUserDefaults] setObject:[self realNameForDisplayableConfigType:self.maybeConfigType] forKey:@"configType"];
    [[NSUserDefaults standardUserDefaults] setBool:self.configShouldConvertFirst forKey:@"configShouldPreprocess"];
    
    [self close];
    
    [[SDConfigLoader sharedConfigLoader] reloadConfig];
}

- (IBAction) whateverIGiveUp:(id)sender {
    [self close];
}

- (NSString*) realNameForDisplayableConfigType:(NSString*)type {
    for (NSString* realName in [self configTypes]) {
        if ([[[self configTypes] objectForKey:realName] isEqual: type])
            return realName;
    }
    return nil;
}

- (NSDictionary*) configTypes {
    return @{@"ruby": @"Ruby",
             @"javascript" : @"JavaScript",
             @"coffeescript" : @"CoffeeScript",
             @"altjs" : @"JS with Conversion"};
}

- (void) show {
    self.maybeConfigName = [[NSUserDefaults standardUserDefaults] stringForKey:@"configPath"];
    self.maybeConfigType = [[self configTypes] objectForKey:[[NSUserDefaults standardUserDefaults] stringForKey:@"configType"]];
    self.maybeConfigConverter = [[NSUserDefaults standardUserDefaults] stringForKey:@"configConverter"];
    self.configShouldConvertFirst = [[NSUserDefaults standardUserDefaults] boolForKey:@"configShouldPreprocess"];
    
    [[self window] center];
    [self showWindow:self];
}

+ (NSSet*) keyPathsForValuesAffectingConfigWindowHeight {
    return [NSSet setWithArray:@[@"configShouldConvertFirst"]];
}

- (CGFloat) configWindowHeight {
    if (self.configShouldConvertFirst)
        return 184;
    else
        return 184 - 22;
}

@end
