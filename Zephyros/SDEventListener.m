//
//  SDEventListener.m
//  Zephyros
//
//  Created by Steven on 4/21/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDEventListener.h"

@interface SDEventListener ()

@property id realObserver;

@end

@implementation SDEventListener

- (void) startListening {
    self.realObserver =
    [[NSNotificationCenter defaultCenter] addObserverForName:[NSString stringWithFormat:@"SD_EVENT_%@", [self.eventName uppercaseString]]
                                                      object:nil
                                                       queue:nil
                                                  usingBlock:^(NSNotification *note) {
                                                      id thing = [[note userInfo] objectForKey:@"thing"];
                                                      self.fn(thing);
                                                  }];
}

- (void) stopListening {
    [[NSNotificationCenter defaultCenter] removeObserver:self.realObserver];
}

@end
