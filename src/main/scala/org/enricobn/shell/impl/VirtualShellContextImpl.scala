package org.enricobn.shell.impl

import org.enricobn.shell.{VirtualShellContext, VirtualShellProfile, VirtualShellProfileRead}

import scala.compiletime.uninitialized

/**
  * Created by enrico on 12/23/16.
  */
class VirtualShellContextImpl() extends VirtualShellContext {
  private var globalProfile : VirtualShellProfile = uninitialized
  private var userProfile : VirtualShellProfile = uninitialized
  private var _profile : VirtualShellProfileRead = uninitialized

  def setGlobalProfile(profile: VirtualShellProfile): Unit = {
    globalProfile = profile

    if (userProfile == null) {
      _profile = profile
    } else {
      _profile = CascadeShellProfile(Seq(globalProfile, userProfile))
    }

  }

  def setUserProfile(profile: VirtualShellProfile): Unit = {
    userProfile = profile

    if (globalProfile == null) {
      _profile = profile
    } else {
      _profile = CascadeShellProfile(Seq(globalProfile, userProfile))
    }

  }

  override def profile: VirtualShellProfileRead = _profile

}
