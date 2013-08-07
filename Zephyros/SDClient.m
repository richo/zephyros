//
//  SDClient.m
//  Zephyros
//
//  Created by Steven Degutis on 7/31/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDClient.h"

#define FOREVER (60*60*24*365)

#import "SDAPI.h"
#import "SDHotKey.h"
#import "SDLogWindowController.h"
#import "SDAlertWindowController.h"
#import "SDConfigLauncher.h"
#import "SDEventListener.h"

@interface SDClient ()

@property int64_t maxRespObjID;
@property NSMutableDictionary* returnedObjects;

@property NSMutableArray* hotkeys;
@property NSMutableArray* listeners;

@end


@implementation SDClient

- (id) init {
    if (self = [super init]) {
        self.hotkeys = [NSMutableArray array];
        self.listeners = [NSMutableArray array];
    }
    return self;
}

- (void) waitForNewMessage {
    [self.sock readDataToData:[@"\n" dataUsingEncoding:NSUTF8StringEncoding]
                  withTimeout:FOREVER
                          tag:0];
}

- (void)socket:(GCDAsyncSocket *)sock didReadData:(NSData *)data withTag:(long)tag {
    if (tag == 0) {
        NSString* str = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        NSInteger size = [str integerValue];
        
        NSString* sizeValidator = [NSString stringWithFormat:@"%ld\n", size];
        
        if (![sizeValidator isEqualToString:str]) {
            [self showAPIError:[NSString stringWithFormat:@"API Error: expected JSON data-load length, got: %@", str]];
            [self waitForNewMessage];
            return;
        }
        
        [self.sock readDataToLength:size
                        withTimeout:FOREVER
                                tag:1];
    }
    else if (tag == 1) {
        NSError* __autoreleasing error;
        id obj = [NSJSONSerialization JSONObjectWithData:data options:0 error:&error];
        
        if (obj == nil) {
            NSString* rawJson = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
            [self showAPIError:[NSString stringWithFormat:@"API Error: expected valid JSON message, got: %@", rawJson]];
            [self waitForNewMessage];
            return;
        }
        
        [self handleMessage:obj];
        [self waitForNewMessage];
    }
}

- (void)socketDidDisconnect:(GCDAsyncSocket *)sock withError:(NSError *)err {
//    NSLog(@"unbinding");
    for (SDHotKey* hotkey in self.hotkeys) {
        [hotkey unbind];
    }
    
    for (SDEventListener* listener in self.listeners) {
        [listener stopListening];
    }
//    NSLog(@"done unbinding");
    
    self.disconnectedHandler(self);
}

- (void) showAPIError:(NSString*)errorStr {
    [[SDLogWindowController sharedLogWindowController] show:errorStr
                                                       type:SDLogMessageTypeError];
}

- (void) handleMessage:(NSArray*)msg {
    if ([msg count] < 3) {
        [self showAPIError:[NSString stringWithFormat:@"API error: invalid message: %@", msg]];
        return;
    }
    
    id msgID = [msg objectAtIndex:0];
    
    if ([msgID isEqual:[NSNull null]]) {
        [self showAPIError:[NSString stringWithFormat:@"API error: invalid message id: %@", msgID]];
        [self sendResponse:nil forID:msgID];
        return;
    }
    
    id recvID = [msg objectAtIndex:1];
    
    NSString* meth = [msg objectAtIndex:2];
    
    if (![meth isKindOfClass:[NSString self]] || [[meth stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]] length] == 0) {
        [self showAPIError:[NSString stringWithFormat:@"API error: invalid method name: %@", meth]];
        return;
    }
    
    NSArray* args = [msg subarrayWithRange:NSMakeRange(3, [msg count] - 3)];
    id recv = [self receiverForID:recvID];
    
    dispatch_async(dispatch_get_main_queue(), ^{
        id result = nil;
        @try {
            result = [self callMethod:meth on:recv args:args msgID:msgID];
        }
        @catch (NSException *exception) {
            [self showAPIError:[exception description]];
        }
        @finally {
            [self sendResponse:result forID:msgID];
        }
    });
}

- (NSNumber*) storeObj:(id)obj {
    if (!self.returnedObjects)
        self.returnedObjects = [NSMutableDictionary dictionary];
    
    self.maxRespObjID++;
    NSNumber* newMaxID = @(self.maxRespObjID);
    
    [self.returnedObjects setObject:obj
                       forKey:newMaxID];
    
    double delayInSeconds = 30.0;
    dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, (int64_t)(delayInSeconds * NSEC_PER_SEC));
    dispatch_after(popTime, dispatch_get_main_queue(), ^(void){
        [self.returnedObjects removeObjectForKey:newMaxID];
    });
    
    return newMaxID;
}

- (id) convertObj:(id)obj {
    if (obj == nil) {
        return [NSNull null];
    }
    else if ([obj isKindOfClass:[NSArray self]]) {
        NSMutableArray* newArray = [NSMutableArray array];
        
        for (id child in obj) {
            [newArray addObject:[self convertObj:child]];
        }
        
        return newArray;
    }
    else if ([obj isKindOfClass:[SDWindowProxy self]] || [obj isKindOfClass:[SDScreenProxy self]] || [obj isKindOfClass:[SDAppProxy self]]) {
        return [self storeObj:obj];
    }
    
    return obj;
}

- (void) sendResponse:(id)result forID:(NSNumber*)msgID {
    [self sendMessage:@[msgID, [self convertObj:result]]];
//    NSLog(@"%@", self.returnedObjects);
}

- (void) sendMessage:(id)msg {
//    NSLog(@"sending [%@]", msg);
    
    NSData* data = [NSJSONSerialization dataWithJSONObject:msg options:0 error:NULL];
    
//    NSString* tempStr = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
    NSString* len = [NSString stringWithFormat:@"%ld", [data length]];
//    NSLog(@"len = %@", len);
//    NSLog(@"data = %@", tempStr);
    [self.sock writeData:[len dataUsingEncoding:NSUTF8StringEncoding] withTimeout:3 tag:0];
    [self.sock writeData:[GCDAsyncSocket LFData] withTimeout:3 tag:0];
    [self.sock writeData:data withTimeout:3 tag:0];
}

- (id) receiverForID:(NSNumber*)recvID {
    if ([recvID isEqual: [NSNull null]])
        return nil;
    
    return [self.returnedObjects objectForKey:recvID];
}

- (NSString*) typeForReceiver:(id)recv {
    if (recv == nil) return @"api";
    if ([recv isKindOfClass:[SDWindowProxy self]]) return @"window";
    if ([recv isKindOfClass:[SDScreenProxy self]]) return @"screen";
    if ([recv isKindOfClass:[SDAppProxy self]]) return @"app";
    @throw [NSException exceptionWithName:@"crap" reason:@"couldn't figure out type for receiver" userInfo:nil];
}

+ (NSDictionary*) methods {
    static NSDictionary* methods;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        methods = @{
                    @"api": @{
                            @"bind": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                SDHotKey* hotkey = [[SDHotKey alloc] init];
                                hotkey.key = [args objectAtIndex:0];
                                hotkey.modifiers = [args objectAtIndex:1];
                                hotkey.fn = ^{
                                    [client sendResponse:nil forID:msgID];
                                };
                                
                                if ([hotkey bind]) {
                                    [client.hotkeys addObject:hotkey];
                                }
                                else {
                                    [client showAPIError:[@"Can't bind: " stringByAppendingString: [hotkey hotKeyDescription]]];
                                }
                                
                                return @-1;
                            },
                            @"listen": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                SDEventListener* listener = [[SDEventListener alloc] init];
                                listener.eventName = [args objectAtIndex:0];
                                listener.fn = ^(id thing) {
                                    [client sendResponse:thing forID:msgID];
                                };
                                
                                [listener startListening];
                                [client.listeners addObject:listener];
                                
                                return @-1;
                            },
                            @"relaunch_config": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                [[SDConfigLauncher sharedConfigLauncher] launchConfigMaybe];
                                return nil;
                            },
                            @"clipboard_contents": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                return [[NSPasteboard generalPasteboard] stringForType:NSPasteboardTypeString];
                            },
                            @"focused_window": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                return [SDWindowProxy focusedWindow];
                            },
                            @"visible_windows": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                return [SDWindowProxy visibleWindows];
                            },
                            @"all_windows": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                return [SDWindowProxy allWindows];
                            },
                            @"main_screen": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                return [SDScreenProxy mainScreen];
                            },
                            @"all_screens": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                return [SDScreenProxy allScreens];
                            },
                            @"running_apps": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                return [SDAppProxy runningApps];
                            },
                            @"alert": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                [[SDAlertWindowController sharedAlertWindowController] show:[args objectAtIndex:0]
                                                                                      delay:[args objectAtIndex:1]];
                                return nil;
                            },
                            @"log": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                [[SDLogWindowController sharedLogWindowController] show:[args objectAtIndex:0]
                                                                                   type:SDLogMessageTypeUser];
                                return nil;
                            },
                            @"choose_from": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                [SDAPI chooseFrom:[args objectAtIndex:0]
                                            title:[args objectAtIndex:1]
                                            lines:[args objectAtIndex:2]
                                            chars:[args objectAtIndex:3]
                                         callback:^(id idx){
                                             [client sendResponse:idx forID:msgID];
                                         }];
                                return @1;
                            },
                            },
                    @"window": @{
                            @"title": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                return [recv title];
                            },
                            @"other_windows_on_same_screen": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                return [recv otherWindowsOnSameScreen];
                            },
                            @"other_windows_on_all_screens": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                return [recv otherWindowsOnAllScreens];
                            },
                            @"set_frame": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                [recv setFrame:[args objectAtIndex:0]];
                                return nil;
                            },
                            @"set_top_left": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                [recv setTopLeft:[args objectAtIndex:0]];
                                return nil;
                            },
                            @"set_size": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                [recv setSize:[args objectAtIndex:0]];
                                return nil;
                            },
                            @"frame": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                return [recv frame];
                            },
                            @"top_left": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                return [recv topLeft];
                            },
                            @"size": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                return [recv size];
                            },
                            @"maximize": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                [recv maximize];
                                return nil;
                            },
                            @"minimize": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                [recv minimize];
                                return nil;
                            },
                            @"un_minimize": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                [recv unMinimize];
                                return nil;
                            },
                            @"app": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                return [recv app];
                            },
                            @"screen": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                return [recv screen];
                            },
                            @"focus_window": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                return [recv focusWindow];
                            },
                            @"focus_window_left": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                [recv focusWindowLeft];
                                return nil;
                            },
                            @"focus_window_right": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                [recv focusWindowRight];
                                return nil;
                            },
                            @"focus_window_up": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                [recv focusWindowUp];
                                return nil;
                            },
                            @"focus_window_down": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                [recv focusWindowDown];
                                return nil;
                            },
                            @"normal_window?": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                return [recv isNormalWindow];
                            },
                            @"minimized?": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                return [recv isWindowMinimized];
                            },
                            },
                    @"app": @{
                            @"all_windows": ^id(SDClient* client, NSNumber* msgID, SDAppProxy* recv, NSArray* args) {
                                return [recv allWindows];
                            },
                            @"visible_windows": ^id(SDClient* client, NSNumber* msgID, SDAppProxy* recv, NSArray* args) {
                                return [recv visibleWindows];
                            },
                            @"title": ^id(SDClient* client, NSNumber* msgID, SDAppProxy* recv, NSArray* args) {
                                return [recv title];
                            },
                            @"hidden?": ^id(SDClient* client, NSNumber* msgID, SDAppProxy* recv, NSArray* args) {
                                return [recv isHidden];
                            },
                            @"show": ^id(SDClient* client, NSNumber* msgID, SDAppProxy* recv, NSArray* args) {
                                [recv show];
                                return nil;
                            },
                            @"hide": ^id(SDClient* client, NSNumber* msgID, SDAppProxy* recv, NSArray* args) {
                                [recv hide];
                                return nil;
                            },
                            @"kill": ^id(SDClient* client, NSNumber* msgID, SDAppProxy* recv, NSArray* args) {
                                [recv kill];
                                return nil;
                            },
                            @"kill9": ^id(SDClient* client, NSNumber* msgID, SDAppProxy* recv, NSArray* args) {
                                [recv kill9];
                                return nil;
                            },
                            },
                    @"screen": @{
                            @"frame_including_dock_and_menu": ^id(SDClient* client, NSNumber* msgID, SDScreenProxy* recv, NSArray* args) {
                                return [recv frameIncludingDockAndMenu];
                            },
                            @"frame_without_dock_or_menu": ^id(SDClient* client, NSNumber* msgID, SDScreenProxy* recv, NSArray* args) {
                                return [recv frameWithoutDockOrMenu];
                            },
                            @"next_screen": ^id(SDClient* client, NSNumber* msgID, SDScreenProxy* recv, NSArray* args) {
                                return [recv nextScreen];
                            },
                            @"previous_screen": ^id(SDClient* client, NSNumber* msgID, SDScreenProxy* recv, NSArray* args) {
                                return [recv previousScreen];
                            },
                            },
                    };
    });
    return methods;
}

- (id) callMethod:(NSString*)meth on:(id)recv args:(NSArray*)args msgID:(NSNumber*)msgID {
//    NSLog(@"recv: %@", recv);
//    NSLog(@"meth: %@", meth);
//    NSLog(@"args: %@", args);
//    NSLog(@"%@", recv);
    
    NSString* type = [self typeForReceiver:recv];
    NSDictionary* methods = [[SDClient methods] objectForKey:type];
    id(^fn)(SDClient* client, NSNumber* msgID, id recv, NSArray* args) = [methods objectForKey:meth];
    
    if (fn)
        return fn(self, msgID, recv, args);
    
    [self showAPIError:[NSString stringWithFormat:@"API Error: Could not find method [%@] on object of type [%@]", meth, type]];
    return nil;
}

@end
