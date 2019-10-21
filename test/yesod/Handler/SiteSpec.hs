module Handler.SiteSpec (spec) where

import Import

import qualified Game.Characters as Characters

spec :: Spec
spec = withApp do
    describe "HomeR" do
        it "responds 200" do
            get HomeR
            statusIs 200

    describe "ChangelogR" do
        it "responds 200" do
            get ChangelogR
            statusIs 200

    describe "GuideR" do
        it "responds 200" do
            get GuideR
            statusIs 200

    describe "CharactersR" do
        it "responds 200" do
            get CharactersR
            statusIs 200

    describe "CharacterR" do
        it "responds 200" do
            get $ CharacterR char
            statusIs 200

    describe "GroupsR" do
        it "responds 200" do
            get GroupsR
            statusIs 200

    describe "MechanicsR" do
        it "responds 200" do
            get MechanicsR
            statusIs 200
  where
    char = headEx Characters.list
