//
//  AppDelegate.m
//  Zephyros
//
//  Created by Steven on 4/13/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "AppDelegate.h"

#import "SDOpenAtLogin.h"
#import "SDConfigLoader.h"
#import "SDAppStalker.h"

#import "SDLogWindowController.h"




//#include <ruby/ruby.h>
//
//
//
//VALUE lolwut(VALUE module, VALUE keys, VALUE callback) {
//    NSString* ss = [NSString stringWithUTF8String:StringValueCStr(keys)];
//    
//    NSLog(@"it was [%@]", ss);
//    
//    double delayInSeconds = 2.0;
//    dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, (int64_t)(delayInSeconds * NSEC_PER_SEC));
//    dispatch_after(popTime, dispatch_get_main_queue(), ^(void){
//        rb_funcall(callback, rb_intern("call"), 0);
//    });
//    
//    
//    NSLog(@"lolcat");
//    return Qnil;
//}



@interface AppDelegate ()

@property NSStatusItem* statusItem;

@end

@implementation AppDelegate

- (void) prepareStatusItem {
    self.statusItem = [[NSStatusBar systemStatusBar] statusItemWithLength:NSVariableStatusItemLength];
    self.statusItem.image = [NSImage imageNamed:@"statusitem"];
    self.statusItem.alternateImage = [NSImage imageNamed:@"statusitem_pressed"];
    self.statusItem.menu = self.statusItemMenu;
    self.statusItem.highlightMode = YES;
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
    [self prepareStatusItem];
    [[SDConfigLoader sharedConfigLoader] prepareScriptingBridge];
    [[SDConfigLoader sharedConfigLoader] reloadConfig];
    [[SDAppStalker sharedAppStalker] beginStalking];
    
    
    
    
    
//    RUBY_INIT_STACK;
//    ruby_init();
//    ruby_init_loadpath();
//    
//    VALUE mymod = rb_define_module("API");
//    rb_define_module_function(mymod, "bind", lolwut, 2);
//    
//    rb_eval_string("API.bind 'sup', lambda { puts 'word' }");
//    
////    rb_require("sum");
//    rb_eval_string("@result = 3");
//    
//    VALUE result = rb_iv_get(rb_eval_string("self"), "@result");
//    printf("Result = %ld\n", NUM2INT(result));
//    
////    return ruby_cleanup(0);
}

- (IBAction) reloadConfig:(id)sender {
    [[SDConfigLoader sharedConfigLoader] reloadConfig];
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
