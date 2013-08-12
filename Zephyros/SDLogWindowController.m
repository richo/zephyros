//
//  SDConfigProblemReporter.m
//  Zephyros
//
//  Created by Steven on 4/13/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDLogWindowController.h"

#import <WebKit/WebKit.h>

#import "SDConfigLauncher.h"

@interface SDLogWindowController ()

@property IBOutlet WebView* webView;
@property (copy) dispatch_block_t beforeReady;
@property BOOL ready;

@end

@implementation SDLogWindowController

+ (SDLogWindowController*) sharedLogWindowController {
    static SDLogWindowController* sharedMessageWindowController;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedMessageWindowController = [[SDLogWindowController alloc] init];
    });
    return sharedMessageWindowController;
}

- (NSString*) windowNibName {
    return @"LogWindow";
}

- (IBAction) clearLog:(id)sender {
    DOMDocument* doc = [self.webView mainFrameDocument];
    [doc body].innerHTML = @"";
}

- (void)webView:(WebView *)sender didFinishLoadForFrame:(WebFrame *)frame {
    self.ready = YES;
    
    if (self.beforeReady) {
        self.beforeReady();
        self.beforeReady = nil;
    }
}

//- (void) windowDidBecomeKey:(NSNotification *)notification {
//    self.window.level = NSNormalWindowLevel;
//}

- (void) windowDidLoad {
    self.window.level = NSFloatingWindowLevel;
    
    self.webView.frameLoadDelegate = self;
    
    NSURL* path = [[NSBundle mainBundle] URLForResource:@"logwindow" withExtension:@"html"];
    NSString* html = [NSString stringWithContentsOfURL:path encoding:NSUTF8StringEncoding error:NULL];
    [[self.webView mainFrame] loadHTMLString:html baseURL:[NSURL URLWithString:@""]];
    
    [[self window] center];
}

- (void) doWhenReady:(dispatch_block_t)blk {
    if (self.ready)
        blk();
    else
        self.beforeReady = blk;
}

- (void) log:(NSString*)message type:(NSString*)type {
    [self window]; // le sigh
    
    [self doWhenReady:^{
        DOMDocument* doc = [self.webView mainFrameDocument];
        
        NSString* classname = [@{SDLogMessageTypeError: @"error",
                               SDLogMessageTypeUser: @"user",
                               SDLogMessageTypeRequest: @"request",
                               SDLogMessageTypeResponse: @"response"} objectForKey:type];
        
        NSDateFormatter* stampFormatter = [[NSDateFormatter alloc] init];
        stampFormatter.dateStyle = NSDateFormatterNoStyle;
        stampFormatter.timeStyle = kCFDateFormatterMediumStyle;
        
        DOMHTMLDivElement* div = (id)[doc createElement:@"div"];
        div.className = classname;
        [[doc body] appendChild:div];
        
        DOMHTMLElement* stamp = (id)[doc createElement:@"small"];
        stamp.innerText = [stampFormatter stringFromDate:[NSDate date]];
        [div appendChild:stamp];
        
        DOMHTMLParagraphElement* p = (id)[doc createElement:@"p"];
        p.innerText = message;
        [div appendChild:p];
        
        [[self.webView windowScriptObject] evaluateWebScript:@"window.scrollTo(0, document.body.scrollHeight);"];
    }];
}

- (void) show:(NSString*)message type:(NSString*)type {
    if (!self.window.isVisible) {
        [self showWindow:nil];
    }
    
    [self log:message type:type];
}

@end
