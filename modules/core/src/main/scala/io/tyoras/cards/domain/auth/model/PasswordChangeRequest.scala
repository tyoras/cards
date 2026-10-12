package io.tyoras.cards.domain.auth.model

import io.tyoras.cards.domain.user.model.User

final case class PasswordChangeRequest(userId: User.ID, oldPassword: Password, newPassword: Password)
