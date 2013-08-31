//
//  SDWindowClientProxy.m
//  Zephyros
//
//  Created by Steven Degutis on 8/12/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDWindowRef.h"

#import "SDGeometry.h"

#import "MACollectionUtilities.h"
#import "SDWindowRef.h"
#import "SDAppRef.h"
#import "SDScreenRef.h"

@implementation SDWindowRef

- (void) dealloc {
    [[self.client undoManager] removeAllActionsWithTarget:self];
}

- (id) title:(NSArray*)args msgID:(id)msgID {
    return [self.resource title];
}

- (id) other_windows_on_same_screen:(NSArray*)args msgID:(id)msgID {
    return MAP([self.resource otherWindowsOnSameScreen], [SDWindowRef store:obj client:self.client]);
}

- (id) other_windows_on_all_screens:(NSArray*)args msgID:(id)msgID {
    return MAP([self.resource otherWindowsOnAllScreens], [SDWindowRef store:obj client:self.client]);
}

- (id) set_frame:(NSArray*)args msgID:(id)msgID {
    NSRect f = [self.resource frame];
    [[self withUndo] set_frame:@[SDDictFromRect(f)]
                         msgID:msgID];
    
    SDTypeCheckArg(NSDictionary, frame, 0);
    [self.resource setFrame: SDRectFromDict(frame)];
    return [NSNull null];
}

- (id) set_top_left:(NSArray*)args msgID:(id)msgID {
    NSPoint tl = [self.resource topLeft];
    [[self withUndo] set_top_left:@[SDDictFromPoint(tl)]
                            msgID:msgID];
    
    SDTypeCheckArg(NSDictionary, top_left, 0);
    [self.resource setTopLeft: SDPointFromDict(top_left)];
    return [NSNull null];
}

- (id) set_size:(NSArray*)args msgID:(id)msgID {
    NSSize s = [self.resource size];
    [[self withUndo] set_size:@[SDDictFromSize(s)]
                        msgID:msgID];
    
    SDTypeCheckArg(NSDictionary, size, 0);
    [self.resource setSize: SDSizeFromDict(size)];
    return [NSNull null];
}

- (id) frame:(NSArray*)args msgID:(id)msgID {
    return SDDictFromRect([self.resource frame]);
}

- (id) top_left:(NSArray*)args msgID:(id)msgID {
    return SDDictFromPoint([self.resource topLeft]);
}

- (id) size:(NSArray*)args msgID:(id)msgID {
    return SDDictFromSize([self.resource size]);
}

- (id) maximize:(NSArray*)args msgID:(id)msgID {
    NSRect f = [self.resource frame];
    [[self withUndo] set_frame:@[SDDictFromRect(f)]
                         msgID:msgID];
    
    [self.resource maximize];
    return [NSNull null];
}

- (id) minimize:(NSArray*)args msgID:(id)msgID {
    [[self withUndo] un_minimize:@[]
                           msgID:msgID];
    
    [self.resource minimize];
    return [NSNull null];
}

- (id) un_minimize:(NSArray*)args msgID:(id)msgID {
    [[self withUndo] minimize:@[]
                        msgID:msgID];
    
    [self.resource unMinimize];
    return [NSNull null];
}

- (id) app:(NSArray*)args msgID:(id)msgID {
    return [SDAppRef store:[self.resource app] client:self.client];
}

- (id) screen:(NSArray*)args msgID:(id)msgID {
    return [SDScreenRef store:[self.resource screen] client:self.client];
}

- (id) focus_window:(NSArray*)args msgID:(id)msgID {
//    SDWindow* win = [SDWindow focusedWindow];
    
//    SDWindowRef* thisRef = [SDWindowRef refWith:]
    
//    [SDWindowProxy focusedWindow]
//    [[self withUndo] un_minimize:@[]
//                           msgID:msgID];
    
    return @([self.resource focusWindow]);
}

- (id) windows_to_north:(NSArray*)args msgID:(id)msgID {
    return MAP([self.resource windowsToNorth], [SDWindowRef store:obj client:self.client]);
}

- (id) windows_to_south:(NSArray*)args msgID:(id)msgID {
    return MAP([self.resource windowsToSouth], [SDWindowRef store:obj client:self.client]);
}

- (id) windows_to_west:(NSArray*)args msgID:(id)msgID {
    return MAP([self.resource windowsToWest], [SDWindowRef store:obj client:self.client]);
}

- (id) windows_to_east:(NSArray*)args msgID:(id)msgID {
    return MAP([self.resource windowsToEast], [SDWindowRef store:obj client:self.client]);
}

- (id) focus_window_left:(NSArray*)args msgID:(id)msgID {
    [self.resource focusWindowLeft];
    return [NSNull null];
}

- (id) focus_window_right:(NSArray*)args msgID:(id)msgID {
    [self.resource focusWindowRight];
    return [NSNull null];
}

- (id) focus_window_up:(NSArray*)args msgID:(id)msgID {
    [self.resource focusWindowUp];
    return [NSNull null];
}

- (id) focus_window_down:(NSArray*)args msgID:(id)msgID {
    [self.resource focusWindowDown];
    return [NSNull null];
}

- (id) normal_window_q:(NSArray*)args msgID:(id)msgID {
    return @([self.resource isNormalWindow]);
}

- (id) minimized_q:(NSArray*)args msgID:(id)msgID {
    return @([self.resource isWindowMinimized]);
}

@end
