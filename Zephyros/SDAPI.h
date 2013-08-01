//
//  SDAPI.h
//  Zephyros
//
//  Created by Steven on 4/15/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "SDWindowProxy.h"
#import "SDScreenProxy.h"


@interface SDAPISettings : NSObject

@property CGFloat alertDisappearDelay;
@property BOOL alertAnimates;
- (NSBox*) alertBox;
- (NSTextField*) alertTextField;

@end

@interface SDAPI : NSObject

+ (SDAPISettings*) settings;
+ (NSDictionary*) shell:(NSString*)cmd args:(NSArray*)args options:(NSDictionary *)options;

+ (void) chooseFrom:(NSArray*)list
              title:(NSString*)title
              lines:(NSNumber*)linesTall
              chars:(NSNumber*)charsWide
           callback:(void(^)(id idx))callback;

//+ (NSDictionary*) shell:(NSString*)cmd args:(NSArray*)args options:(NSDictionary *)options;
// this is already in python, go, node.js, ruby. what lang doesnt have it? do we even need it?

@end
